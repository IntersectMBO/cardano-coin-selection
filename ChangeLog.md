## [1.0.1] - 2020-05-13

### Improvements

* Adjusted the Largest-First algorithm to pay for outputs collectively instead
  of individually.

  The updated algorithm should now be successful at paying for any set of
  outputs of total value **_v_** provided that the total value **_u_** of
  available inputs satisfies **_u_** ≥ **_v_**.

  The cardinality restriction requiring the number of inputs to be greater than
  the number of outputs has been removed.

  See the following commits for more details:

  * `aae26dddb727779f`
    ([PR #73](https://github.com/input-output-hk/cardano-coin-selection/pull/73))
  * `65d5108bac63251f`
    ([PR #76](https://github.com/input-output-hk/cardano-coin-selection/pull/76))

### Fixes

* Fixed a small issue with the migration algorithm that caused it to
  occasionally return more change than actually available.

  This issue only occurred in extreme situations, where the total value of the
  available UTxO set was less than the dust threshold value.

  See the following commits for more details:

  * `14ef17a9647974a8`
    ([PR #77](https://github.com/input-output-hk/cardano-coin-selection/pull/77))

## [1.0.0] - 2020-04-29

Initial release.
