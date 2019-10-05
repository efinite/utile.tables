# utile.tables
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/utile.tables)](https://CRAN.R-project.org/package=utile.tables)
[![Total Downloads](https://cranlogs.r-pkg.org/badges/grand-total/utile.tables)](https://CRAN.R-project.org/package=utile.tables)

## Overview
A collection of functions to make building customized ready-to-export tables for publication purposes easier and creating summaries of large datasets for review a breeze. Includes methods for automatically building a table from data or building the table row-by-row.

## Functions
### > "build_"
- `build_row()`: Generates a summarizing table row from a column of data.
- `build_row_()`: Creates a copy of `build_row()` with built in data and pre-specified options for row formatting.
- `build_table()`: Summarizes multiple columns of data as rows in a table. Can work in an automated fashion or with manually specified options. It is essentially a wrapper for `build_row()`.
- `build_footer()`: Creates a footer row specifying types of summary statistics and tests used.

### > "build_event_"
- `build_event_row()`: Generates a summarizing table row for a parameter (column) in a time-to-event model.
- `build_event_row_()`: Creats a copy of `build_event_row()` with a built-in fit and pre-specified options for row formating.
- `build_event_table()`: Summarizes multiple parameters (columns) in a time-to-event model. Can work in an automated fashion or with manually specified options. Also supports both univariate and multivariate approaches. It is essentially a wrapper for `build_event_row()`.
