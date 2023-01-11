# Development (0.3.0)
* BREAKING: Revision of how `build_row` methods handle and display NA data. Missing data are
now made explicit for factor data to ensure counts sum appropriately. The `na.rm=`
parameter has also been removed to reduce ambiguity.
* Fixed several counting errors in `build_row` which would occur in certain circumstances
when NA values were present in input data.
* With the introduction of the `col.overall=` argument, the "Overall" column produced 
by build_row is now optional.
* BREAKING: Many arguments for both `build_table` and `build_row` have been renamed
to make their effects clearer.
* BREAKING: The `parametric=` argument has been replaced by the `stat=` and
`test=` arguments which allow manual specification.
* Support for and requirement of `utile.tools::` version 0.3.0 which introduces
breaking changes to the `test_hypothesis` function.
* Preemptive removal of the recently superseded `purrr::imap_dfr`. Lists are now
concatenated with `purrr::list_rbind`.

# 0.2.2
* Fixed CRAN LazyData error

# 0.2.1
* Removed methods dependency, opting for base R's `match.call`.
* Micro-optimizations to performance of the `build_row` and `build_table` functions.
* Fixed character casting issue in `build_table.coxph` that would cause p-value rounding to fail when the p-value was in scientific notation.
* Fixed broken filter in `build_model` that was not removing existing model terms from the list of interest.
* Added re-exports for the tidyselect helpers.

# 0.2.0
* Replaced `build_event_table()` & `build_event_rows()` with `build_model()` which has an S3 method for coxph models and uses tidyselect syntax. Plain formulas and survfit objects are no longer supported.
* `build_table()` was converted to an S3 method that now uses tidyselect syntax which is far easier to use. The data.frame method now works more than 7.5x faster than the prior version. This function is the new home of methods from `utile.tools::tabulate_model()`.
* `build_row()` was replaced with an S3 method that directly exposes the previously internal functions for working with raw factor, logical, or numeric data.
* Support for `utile.tools::` version 0.2.5 was added.

# 0.1.8
* Upgraded backend functions to be compatible with `utile.tools::` version 0.2.3 

# 0.1.7
* Added graceful handling of NULL table data to `build_event_row()`.
* Tables created with `build_event_` now return a unified hazard ratio and confidence interval column for behavior more consistent with `build_row()`/`build_table()`.

# 0.1.6
* Fixed logic in `build_event_row_()` to return the reference table when a parameter is skipped.

# 0.1.5
* Addressed `tibble::is.tibble()` depreciation.
* Backend function changes:
  - `.row_coxph()`: Rewrote to work with `utile.tools::tabulate_model()` version 0.2.0.
  - `.row_` Functions
    - Rewrote to reduce redundancy and improve efficiency.
    - Removed redundant code from calls of `utile.tools::paste_freq()`.

# 0.1.4
* First public release
