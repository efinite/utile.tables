# utile.tables 0.3.0

## Breaking Changes

* `build_row()` S3 methods now handle and display `NA` data differently. Missing
data are now made explicit for factor data to ensure counts sum appropriately.
The `na.rm` parameter has also been removed to reduce ambiguity.

* Arguments for nearly all functions have been renamed to make their
effects clearer.

* In all functions, the `parametric` argument has been replaced by the
`stat` and `test` arguments to allow manual selection of tests and provide greater
transparency.

## New Features

* New `col.overall` argument in `build_row()` to allow for removal of the "Overall"
column.

* `build_row()` labels are now more flexible and can accommodate no input more
gracefully.

## Fixes & Minor Changes

* Fixes for several `build_row()` counting errors which would occur in certain
circumstances when `NA` values were present in input data.

* Support for and requirement of `utile.tools` version 0.3.0 which introduced
breaking changes to the `utile.tools::test_hypothesis()` function.

* Preemptive removal of the recently superseded `purrr::imap_dfr()`. Lists are now
concatenated with `purrr::list_rbind()`. As a result, `purrr` version 1.0.0 or
greater is now required.

* Added a `pkgdown` website.


# utile.tables 0.2.2

* Fixed CRAN LazyData error


# utile.tables 0.2.1

* Removed methods dependency, opting for base R's `match.call()`.

* Micro-optimizations to performance of the `build_row()` and `build_table()` functions.

* Fixed character casting issue in `build_table.coxph()` that would cause p-value rounding to fail when the p-value was in scientific notation.

* Fixed broken filter in `build_model()` that was not removing existing model terms from the list of interest.

* Added re-exports for the tidyselect helpers.


# utile.tables 0.2.0

* Replaced `build_event_table()` & `build_event_rows()` with `build_model()` which has an S3 method for coxph models and uses tidyselect syntax. Plain formulas and survfit objects are no longer supported.

* `build_table()` was converted to an S3 method that now uses tidyselect syntax which is far easier to use. The data.frame method now works more than 7.5x faster than the prior version. This function is the new home of methods from `utile.tools::tabulate_model()`.

* `build_row()` was replaced with an S3 method that directly exposes the previously internal functions for working with raw factor, logical, or numeric data.

* Support for `utile.tools::` version 0.2.5 was added.


# utile.tables 0.1.8

* Upgraded backend functions to be compatible with `utile.tools::` version 0.2.3 


# utile.tables 0.1.7

* Added graceful handling of NULL table data to `build_event_row()`.

* Tables created with `build_event_` now return a unified hazard ratio and confidence interval column for behavior more consistent with `build_row()`/`build_table()`.


# utile.tables 0.1.6
* Fixed logic in `build_event_row_()` to return the reference table when a parameter is skipped.


# utile.tables 0.1.5
* Addressed `tibble::is.tibble()` depreciation.

* Backend function changes:
  - `.row_coxph()`: Rewrote to work with `utile.tools::tabulate_model()` version 0.2.0.
  - `.row_` Functions
    - Rewrote to reduce redundancy and improve efficiency.
    - Removed redundant code from calls of `utile.tools::paste_freq()`.


# utile.tables 0.1.4
* First public release
