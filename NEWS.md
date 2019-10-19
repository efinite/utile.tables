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
