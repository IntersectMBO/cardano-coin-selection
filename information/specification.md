# Coin Selection Specification: Draft 1

| Caution!                                                                                            |
|-----------------------------------------------------------------------------------------------------|
| This is a draft version of the specification.<br>Several details may differ from the final version. |

# Limitations

This draft has several **limitations**, to be addressed in future versions:

* There is currently no description of the algorithm used for **migration** of
  funds from one wallet to another.
* Descriptions of **fee balancing** algorithms are not yet included.
* Not all **properties** of coin selection algorithms are included.
* There are currently no **worked examples** or **test vectors**.

# Contents

* [Purpose](#purpose)
* [Background](#background)
  * [What is Coin Selection?](#what-is-coin-selection)
  * [Why is Coin Selection Non-Trivial?](#why-is-coin-selection-non-trivial)
    * [Issues](#issues)
    * [Goals](#goals)
* [Definitions](#definitions)
  * [Address](#address)
  * [Coin Amount](#coin-amount)
  * [Transaction Input](#transaction-input)
  * [Transaction Output](#transaction-output)
  * [UTxO Set](#utxo-set)
* [Interface](#interface)
  * [Parameters](#parameters)
    * [Requested Output List](#requested-output-list)
    * [Initial UTxO Set](#initial-utxo-set)
    * [Maximum Input Count](#maximum-input-count)
  * [Results](#results)
    * [Coin Selection](#coin-selection)
    * [Remaining UTxO Set](#remaining-utxo-set)
  * [Properties](#properties)
    * [Coverage of Payments](#coverage-of-payments)
    * [Correctness of Change](#correctness-of-change)
    * [Conservation of UTxO](#conservation-of-utxo)
    * [Conservation of Outputs](#conservation-of-outputs)
    * [Cardinality](#cardinality)
  * [Failure Modes](#failure-modes)
    * [UTxO Balance Insufficient](#utxo-balance-insufficient)
    * [UTxO Not Fragmented Enough](#utxo-not-fragmented-enough)
    * [UTxO Fully Depleted](#utxo-fully-depleted)
    * [Maximum Input Count Exceeded](#maximum-input-count-exceeded)
* [Implementations](#implementations)
  * [Largest-First](#largest-first)
    * [State](#state)
      * [Available UTxO List](#available-utxo-list)
      * [Unpaid Output List](#unpaid-output-list)
      * [Accumulated Coin Selection](#accumulated-coin-selection)
    * [Computation](#computation)
    * [Termination](#termination)
  * [Random-Improve](#random-improve)
    * [Motivating Principles](#motivating-principles)
      * [Principle 1: Dust Management](#principle-1-dust-management)
      * [Principle 2: Change Management](#principle-2-change-management)
      * [Principle 3: Performance Management](#principle-3-performance-management)
    * [State](#state-1)
      * [Available UTxO Set](#available-utxo-set)
      * [Accumulated Coin Selection](#accumulated-coin-selection-1)
    * [Computation](#computation-1)
      * [Phase 1: Random Selection](#phase-1-random-selection)
      * [Phase 2: Improvement](#phase-2-improvement)
    * [Termination](#termination-1)
* [External Resources](#external-resources)
  * [Self Organization in Coin Selection](#self-organization-in-coin-selection)

# Purpose

The purpose of this article is to describe, in _human-readable terms_, the coin
selection algorithms used by Cardano Wallet and other parts of the Cardano
ecosystem.

# Background

## What is Coin Selection?

Coin selection is the process of choosing unspent coins from a wallet in order
to pay money to one or more recipients.

In the world of physical money, wallets hold value in the form of _coins and
banknotes_.  When making a payment, we typically select a number of coins and
banknotes from a wallet that, when added together, have enough value to cover
the amount required.  Ideally, we'd always be able to select _just enough_ to
cover the exact amount.  However, given that coins and banknotes have fixed
values (and cannot be subdivided), it's often _impossible_ to select the exact
amount required. In such cases, we typically give the recipient _more_ than the
required amount, and then receive the excess value back as _change_.

Similarly to how a physical wallet holds value in the form of unspent coins and
banknotes, a Cardano wallet holds value in the form of a set of _unspent
transaction outputs_ (UTxO), the results of previous transactions that
transferred money to the wallet. Each output in the UTxO set has a particular
_value_, and the total value of a wallet is the _sum_ of these values.

When using a Cardano wallet to make a payment, the wallet software must choose
unspent outputs from the wallet's UTxO set, so that the total value of selected
outputs is enough to cover the target amount. Just as with physical money,
outputs from the UTxO cannot be subdivided, and must be spent completely in a
given transaction.  Consequently, in the case where it's not possible to cover
the target amount exactly, the wallet software must arrange that change is paid
back to the wallet.

Coin selection refers to the process of choosing _unspent outputs_ from a
wallet's UTxO set in order to make one or more payments, and arranging for
_change_ to be paid back to the wallet by creating one or more _change
outputs_.

## Why is Coin Selection Non-Trivial?

### Issues

There are a number of issues which make the problem of coin selection more
complicated than it would initially appear.

 * Each transaction has a _maximum size_, as defined by the protocol. The size
   of a transaction increases as we add more inputs or outputs.

   Therefore, there's a practical limit on the number of coins we can select
   for any given transaction.

 * The most obvious strategy for coin selection, which consists of trying to
   get as close to the requested value as possible, will tend (over time) to
   create a lot of _dust_: small unspent outputs.

   Dust outputs are a problem, because even if the total value of dust in a
   wallet is more than enough to cover a given target amount, if we attempt to
   include that dust in a given transaction, we may run out of space (by
   reaching the transaction size limit) before we can cover the target amount.

 * The most obvious strategy for paying change, which consists of making a
   single change output with the exact excess value, will tend (over time) to
   reduce the size of the UTxO set. This is bad for two reasons:

    1. Having a small UTxO set limits the number of payments that we can make
       in parallel. Since a single UTxO entry can only be used to pay for a
       single output, we need at least as many UTxO entries as there are
       outputs.

    2. The approach of coalescing all change into a single output is widely
       considered to have negative privacy implications. The discussion of
       privacy issues is beyond the scope of this article.

### Goals

There are several desirable properties that we would like a coin selection
algorithm to have. These properties include:

 * A coin selection algorithm should, over the course of time, aim to generate
   and maintain a UTxO set with _useful_ outputs: that is, outputs that allow
   us to process future payments with a _minimum_ number of inputs.

 * A coin selection algorithm should employ strategies to limit the
   amount of _dust_ that accumulates in the UTxO set.

For more information on dust avoidance, see [Self Organisation in Coin
Selection](#self-organization-in-coin-selection).

# Definitions

### Address

An _address_ is a unique identifier that represents a destination for a payment.

Addresses are owned (and generated) by individual wallets.

In general, coin selection algorithms are agnostic to the choice of address
format. Therefore, the details of individual address formats are not described
within this document.

### Coin Amount

A _coin amount_ is a positive integer value that represents a number of
[Lovelace](https://cardanodocs.com/cardano/monetary-policy/).

One [Ada](https://cardanodocs.com/cardano/monetary-policy/) is _exactly_ equal
to one million Lovelace.

### UTxO Set

A _UTxO set_ represents the unspent value associated with a wallet.

Each member of the set is a pair of the form (**_i_**, **_o_**), where:

  * **_i_** is a [transaction input](#transaction-input);
  * **_o_** is a [transaction output](#transaction-output).

A given [transaction input](#transaction-input) can only appear _once_ in a
given UTxO set.

### Transaction Input

A _transaction input_ is a pair of values (**_h_**, **_n_**), where:

  * **_h_** is the hash of an existing transaction **_t_**;
  * **_n_** is a 0-based integer index into the output list of transaction
    **_t_**.

### Transaction Output

A _transaction output_ is a pair of values (**_t_**, **_a_**), where:

  * **_t_** is a [target address](#address);
  * **_a_** is a [coin amount](#coin-amount).

# Interface

All coin selection algorithms used by Cardano Wallet implement a _common
interface_.

Essentially, a coin selection algorithm is simply a _mathematical function_
that, when applied to a [standard set of parameters](#parameters), will produce
a [standard set of results](#results).

This section defines the [parameters](#parameters) accepted by coin selection
algorithms used in Cardano Wallet, the [results](#results) returned, as well as
the various [error conditions](#failure-modes) that may occur.

In this section, the terms _coin selection algorithm_ and _coin selection
function_ will be used interchangeably.

## Parameters

A coin selection function accepts the following parameters:

 1. #### Requested Output List

    A list of payments to be made to recipient addresses, encoded as a list of
    [transaction outputs](#transaction-output).

 2. #### Initial UTxO Set

    A subset of a wallet's [UTxO set](#utxo-set).

    The coin selection algorithm will select entries from within this set in
    order to cover payments listed in the [requested output
    list](#requested-output-list).

    Normally, this parameter would be assigned with the complete [UTxO
    set](#utxo-set) of a wallet, giving the coin selection algorithm access to
    the total value associated with that wallet.

 3. #### Maximum Input Count

    An _upper bound_ on the number of UTxO entries that the coin selection
    algorithm is permitted to select from the [initial UTxO
    set](#initial-utxo-set).

## Results

A coin selection function produces the following result values:

 1. #### Coin Selection

    The _coin selection_ is a record with three fields:

      * A set of **_inputs_**, equivalent to a subset of the
        [initial UTxO set](#initial-utxo-set).

        Represents the value that has been selected from the wallet in order to
        cover the total payment value.

      * A set of **_outputs_** (see [transaction output](#transaction-output)).

        Represents the set of payments to be made to recipient addresses.

      * A set of **_change values_**, where each change value is simply a
        [coin amount](#coin-amount).

        Represents the change to be returned to the wallet.

 2. #### Remaining UTxO Set

    The _remaining UTxO set_ is a subset of the [initial UTxO
    set](#initial-utxo-set).

    It represents the set of values that remain after the coin selection
    algorithm has removed values to pay for entries in the [requested output
    list](#requested-output-list).

    If a coin selection algorithm is applied to the _complete_ UTxO set of a
    wallet, then the remaining UTxO set represents the _updated_ UTxO set of the
    wallet.

## Properties

### Coverage of Payments

This property states that the total value of the [coin
selection](#coin-selection) result is sufficient to _cover_ the total value of
the [requested output list](#requested-output-list).

In particular:

  * V<sub>_selected_</sub> ≥ V<sub>_requested_</sub>

Where:

  * V<sub>_requested_</sub>

    is the total value of the [requested output list](#requested-output-list)


  * V<sub>_selected_</sub>

    is the total value of the _inputs_ field of the [coin
    selection](#coin-selection) result.

### Correctness of Change

This property states that the correct amount of _change_ was generated.

In particular:

  * V<sub>_selected_</sub> = V<sub>_requested_</sub> + V<sub>_change_</sub>

Where:

  * V<sub>_change_</sub>

    is the total value of the _change_ field of the [coin
    selection](#coin-selection) result.

  * V<sub>_requested_</sub>

    is the total value of the [requested output list](#requested-output-list)

  * V<sub>_selected_</sub>

    is the total value of the _inputs_ field of the [coin
    selection](#coin-selection) result.

### Conservation of UTxO

This property states that the [initial UTxO set](#initial-utxo-set) is
_conserved_ in the [results](#results).

There are two mutually-exclusive cases:

  * If a UTxO entry is _selected_ by the coin selection algorithm, it is
    included in the _inputs_ field of the [coin selection](#coin-selection)
    result.

  * If a UTxO entry is _not selected_ by the coin selection algorithm, it is
    included in the [remaining UTxO set](#remaining-utxo-set) result.

The following laws hold:

  * UTxO<sub>_selected_</sub> ⊆ UTxO<sub>_initial_</sub>
  * UTxO<sub>_remaining_</sub> ⊂ UTxO<sub>_initial_</sub>
  * UTxO<sub>_remaining_</sub> ∩ UTxO<sub>_selected_</sub> = ∅
  * UTxO<sub>_remaining_</sub> ⋃ UTxO<sub>_selected_</sub> =
    UTxO<sub>_initial_</sub>

Where:

  * **UTxO<sub>_initial_</sub>**

    is the [initial UTxO set](#initial-utxo-set).

  * **UTxO<sub>_remaining_</sub>**

    is the [remaining UTxO set](#remaining-utxo-set).

  * **UTxO<sub>_selected_</sub>**

    is the value of the _inputs_ field of the [coin selection](#coin-selection)
    result.

### Conservation of Outputs

This property states that the [requested output list](#requested-output-list)
is _conserved_ in the [coin selection](#coin-selection) result.

In particular, the _outputs_ field of the [coin selection](#coin-selection)
result should be _equal to_ the [requested output list](#requested-output-list).

### Cardinality

All algorithms require that:

 1. Each output from the [requested output list](#requested-output-list) is
    paid for by _one or more_ entries from the
    [initial UTxO set](#initial-utxo-set).

 2. Each entry from the [initial UTxO set](#initial-utxo-set) is used to pay
    for _at most one_ output from the [requested output
    list](#requested-output-list).

## Failure Modes

There are a number of ways in which a coin selection algorithm can fail:

  * #### UTxO Balance Insufficient

    This failure occurs when the total value of the entries within the [initial
    UTxO set](#initial-utxo-set) (the amount of money _available_) is _less
    than_ the the total value of all entries in the [requested output
    list](#requested-output-list) (the amount of money _required_).

  * #### UTxO Not Fragmented Enough

    This failure occurs when the _number_ of entries in the [initial UTxO
    set](#initial-utxo-set) is _smaller than_ the number of entries in the
    [requested output list](#requested-output-list).

    All algorithms require that there is _at least one_ UTxO entry available
    _for each_ requested output.

  * #### UTxO Fully Depleted

    This failure occurs if the algorithm depletes all entries from the [initial
    UTxO set](#initial-utxo-set) _before_ it is able to pay for all outputs in
    the [requested output list](#requested-output-list).

  * #### Maximum Input Count Exceeded

    This failure occurs if the _number_ of UTxO entries needed to pay for the
    outputs in the [requested output list](#requested-output-list) exceeds the
    upper limit specified by the [maximum input count](#maximum-input-count)
    parameter.

# Implementations

This section will describe the coin selection algorithms used by Cardano Wallet.

These algorithms implement a [common interface](#interface), as described
above.

There are two main algorithms used by Cardano Wallet:

  * [Largest-First](#largest-first)
  * [Random-Improve](#random-improve)

In general, Cardano Wallet gives _priority_ to the
[Random-Improve](#random-improve) algorithm.

However, in the case where [Random-Improve](#random-improve) is unable to
produce a result, Cardano Wallet will fall back to the
[Largest-First](#largest-first) algorithm.

## Largest-First

The **Largest-First** coin selection algorithm processes
[outputs](#requested-output-list) in _descending order of coin value_, from
_largest_ to _smallest_.

For each output, it repeatedly selects the _largest remaining_ unspent UTxO
entry until the value of selected entries is greater than or equal to the
value of that output.

The name of the algorithm is taken from the idea that the **largest** UTxO
entry is always considered **first**.

### State

At all stages of processing, the algorithm maintains the following pieces of
state:

 1. #### Available UTxO List

    This is initially equal to the [initial UTxO set](#initial-utxo-set),
    sorted into _descending order of coin value_.

    The _head_ of the list is always the remaining UTxO entry with the _largest
    coin value_.

    Entries are incrementally removed from the _head_ of the list as the
    algorithm proceeds, until the list is empty.

 2. #### Unpaid Output List

    This is initially equal to the [requested output
    list](#requested-output-list), sorted into _descending order of coin
    value_.

    The _head_ of the list is always the unpaid output with the _largest coin
    value_.

    Entries are incrementally removed from the _head_ of the list as the
    algorithm proceeds, until the list is empty.

 3. #### Accumulated Coin Selection

    The accumulated coin selection is a [coin selection](#coin-selection) where
    all fields are initially equal to the _empty set_.

### Computation

The algorithm proceeds according to the following sequence of steps:

 * **Step 1**

   Remove a single _unpaid output_ from the head of the [unpaid output
   list](#unpaid-output-list).

 * **Step 2**

   Repeatedly remove UTxO entries from the head of the [available UTxO
   list](#available-utxo-list) until the total value of entries removed is
   _greater than or equal to_ the value of the _removed output_.

 * **Step 3**

   Use the _removed UTxO entries_ to pay for the _removed output_.

   This is achieved by:

    * adding the _removed UTxO entries_ to the _inputs_ field of the
      [accumulated coin selection](#accumulated-coin-selection).
    * adding the _removed output_ to the _outputs_ field of the
      [accumulated coin selection](#accumulated-coin-selection).

 * **Step 4**

   If the _total value_ of the _removed UTxO entries_ is greater than the
   value of the _removed output_, generate a coin whose value is equal to
   the exact difference, and add it to the _change values_ field of the
   [accumulated coin selection](#accumulated-coin-selection).

 * **Step 5**

   If the _unpaid output list_ is empty, **terminate** here.

   Otherwise, return to **Step 1**.

### Termination

The algorithm terminates _successfully_ if the [available UTxO
list](#available-utxo-list) is not depleted before the [unpaid output
list](#unpaid-output-list) can be fully depleted (i.e., if all the outputs have
been paid for).

The [accumulated coin selection](#accumulated-coin-selection) is returned
to the caller as the [coin selection](#coin-selection) result.

The [available UTxO list](#available-utxo-list) is returned to the caller
as the [remaining UTxO set](#remaining-utxo-set) result.

## Random-Improve

The **Random-Improve** coin selection algorithm works in _two phases_.

In the _first_ phase, the algorithm iterates through each of the [requested
outputs](#requested-output-list) in _descending order of coin value_, from
_largest_ to _smallest_. For each output, the algorithm repeatedly selects
entries at **random** from the [initial UTxO set](#initial-utxo-set), until the
_total value_ of selected entries is enough to pay for that ouput.

In the _second_ phase, the algorithm attempts to **improve** upon each of the
UTxO selections made in the previous phase, by conservatively expanding the
selection made for each output, in order to generate improved change values.

The name of the algorithm is taken from the two-phase process of selecting
UTxO entries at **random**, and then attempting to **improve** upon the
selections.

### Motivating Principles

There are several motivating principles behind the design of the algorithm.

#### Principle 1: Dust Management

The probability that random selection will choose dust entries from a UTxO
set _increases_ with the proportion of dust in the set.

Therefore, for a UTxO set with a large amount of dust, there's a high
probability that a random subset will include a large amount of dust.

Over time, selecting entries randomly in this way will tend to _limit_ the
amount of dust that accumulates in the UTxO set.

#### Principle 2: Change Management

As mentioned in the [Goals](#goals) section, it is desirable that coin
selection algorithms, over time, are able to create UTxO sets that have
_useful_ outputs: outputs that will allow us to process future payments with a
minimum number of inputs.

If for each payment request of value **v** we create a change output of
_roughly_ the same value **v**, then we will end up with a distribution of
change values that matches the typical value distribution of payment
requests.

#### Principle 3: Performance Management

Searching the UTxO set for additional entries to _improve_ our change outputs
is _only_ useful if the UTxO set contains entries that are sufficiently
small enough. But it is precisely when the UTxO set contains many small
entries that it is less likely for a randomly-chosen UTxO entry to push the
total above the upper bound.

### State

At all stages of processing, the algorithm maintains the following pieces of
state:

 1. #### Available UTxO Set

    This is initially equal to the [initial UTxO set](#initial-utxo-set).

 2. #### Accumulated Coin Selection

    The accumulated coin selection is a [coin selection](#coin-selection) where
    all fields are initially equal to the _empty set_.

### Computation

The algorithm proceeds in two phases.

#### Phase 1: Random Selection

In this phase, the algorithm iterates through each of the [requested
outputs](#requested-output-list) in descending order of coin value, from
largest to smallest.

For each output of value **v**, the algorithm repeatedly selects entries at
**random** from the [available UTxO set](#available-utxo-set), until the _total
value_ of selected entries is greater than or equal to **v**. The selected
entries are then _associated with_ that output, and _removed_ from the
[available UTxO set](#available-utxo-set).

This phase ends when _every_ output has been associated with a selection of
UTxO entries.

#### Phase 2: Improvement

In this phase, the algorithm attempts to improve upon each of the UTxO
selections made in the previous phase, by conservatively expanding the
selection made for each output in order to generate improved change
values.

During this phase, the algorithm:

  * processes outputs in _ascending order of coin value_.

  * continues to select values from the [available UTxO
    set](#available-utxo-set).

  * incrementally populates the
    [accumulated coin selection](#accumulated-coin-selection-1).

For each output of value **_v_**, the algorithm:

 1.  **Calculates a _target range_** for the total value of inputs used to
     pay for that output, defined by the triplet:

     (_minimum_, _ideal_, _maximum_) = (_v_, _2v_, _3v_)

 2.  **Attempts to improve upon the existing UTxO selection** for that output,
     by repeatedly selecting additional entries at random from the [available
     UTxO set](#available-utxo-set), stopping when the selection can be
     improved upon no further.

     A selection with value _v1_ is considered to be an _improvement_ over a
     selection with value _v0_ if **all** of the following conditions are
     satisfied:

      * **Condition 1**: we have moved closer to the _ideal_ value:

        abs (_ideal_ − _v1_) < abs (_ideal_ − _v0_)

      * **Condition 2**: we have not exceeded the _maximum_ value:

        _v1_ ≤ _maximum_

      * **Condition 3**: when counting cumulatively across all outputs
        considered so far, we have not selected more than the _maximum_ number
        of UTxO entries specified by [Maximum Input
        Count](#maximum-input-count).

 3.  **Creates a _change value_** for the output, equal to the total value
     of the _improved UTxO selection_ for that output minus the value _v_ of
     that output.

 4.  **Updates the [accumulated coin
     selection](#accumulated-coin-selection-1)**:

      * Adds the _output_ to the _outputs_ field;
      * Adds the _improved UTxO selection_ to the _inputs_ field;
      * Adds the _change value_ to the _change values_ field.

This phase ends when every output has been processed, **or** when the
[available UTxO set](#available-utxo-set) has been exhausted, whichever occurs
sooner.

### Termination

When both phases are complete, the algorithm terminates.

The [accumulated coin selection](#accumulated-coin-selection-1) is returned
to the caller as the [coin selection](#coin-selection) result.

The [available UTxO set](#available-utxo-set) is returned to the caller as the
[remaining UTxO set](#remaining-utxo-set) result.

# External Resources

## Self Organization in Coin Selection

https://cardanofoundation.org/en/news/self-organisation-in-coin-selection/
