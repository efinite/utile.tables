utils::globalVariables(c(
  '.data.default', '.by', '.parametric',
  '.inverse', '.indent', '.less.than.one',
  '.label.stats', '.remove.na', '.p.digits',
  '.digits', '.percent.sign', '.args'
))

#' @title build_row
#' @description Generates a summarizing table row from column data.
#' @param .table Optional. Tibble. A tibble to append row data to.
#' @param label Optional. Character. Label for the row (i.e. 'Age, years'). Defaults to value of \'col\'.
#' @param col Optional. Character. Name of column to be summarized. If left blank, either a
#' frequency row will be created (if 'label' not specified) or empty label row created
#' (if 'label' specified).
#' @param by Optional. Character. Name of factor or logical column to stratify by. If using
#' build_row_(), this may be pre-specified.
#' @param data Required/Optional. Tibble or Character. Contains data to summarize.
#' If using build_row_() to pre-load data, you may instead provide a character
#' string of code that would represent how you would reference the tibble
#' (i.e.  '.index' or '.index %>% filter(my_subgroup)'). If, while using build_row_(),
#' you also specified a default tibble to be used via data.default, that tibble will
#' automatically be used as a fallback if the 'data' parameter is left blank.
#' @param parametric Optional. Logical. Indicates parametric testing should be used for comparisons
#' (Fisher's exact and Student's Unpaired T-Test). Defaults to FALSE (non-parametric; Chi-squared and
#' Wilcox Rank-sum).
#' @param inverse Optional. Logical. Indicates to summarize the FALSE/No data of a logical column
#' (i.e. 'Smoking Hx, yes' -> 'Smoking Hx, no'). Defaults to FALSE (Summarizes TRUE/Yes data).
#' @param indent Optional. Logical. Indent a variable's label. Defaults to FALSE.
#' @param percent.sign Optional. Logical. Indicates percent sign should be printed for
#' frequencies. Defaults to TRUE.
#' @param less.than.one Optional. Logical. Indicates means/medians that round to 0 should be
#' printed as <1 (i.e. <1 [0-4]). Defaults to FALSE (0).
#' @param label.stats Optional. Logical. Whether to append the type of statistic (median, n, mean)
#' to the row's label. Defaults to TRUE.
#' @param remove.na Optional. Logical. Remove NA from denominator in frequency calculations.
#' Defaults to TRUE.
#' @param digits Optional. Integer. Number of digits to round numerics to. Defaults to 1.
#' @param p.digits Optional. Integer. Number of p-value digits to print. Note
#' that p-values are still rounded based on \'digits\' parameter. Defaults to 4.
#' @param ... Optional. Any other variables or tibbles you want to make available for use
#' as row data. Recommend naming each of these with a starting '.' to ensure they
#' do not conflict with other variables (i.e. build_row(.index = data.index,
#' .radiology = data.radiology)).
#' @return Data is returned in the form of a tibble containing the row(s).
#' @examples
#' library(dplyr)
#'
#' data_mtcars <- datasets::mtcars %>%
#'   as_tibble() %>%
#'   mutate_at(vars('vs', 'am'), as.logical) %>%
#'   mutate_at(vars('gear', 'carb', 'cyl'), as.factor)
#'
#' # Stand-alone row
#' build_row(
#'   label = 'Gears',
#'   col = 'gear',
#'   by = 'am',
#'   data = data_mtcars,
#'   percent.sign = FALSE
#' )
#'
#' # Summary Table
#' build_row(label = 'Miles per gallon', col = 'mpg', data = data_mtcars) %>%
#' build_row(label = 'Cylinders', col = 'cyl', data = data_mtcars) %>%
#' build_row(label = 'Horsepower', col = 'hp', data = data_mtcars)
#' @export
build_row <- function(
  .table = NULL, label = NULL, col = NULL,
  by = NULL, data = NULL, parametric = FALSE,
  inverse = FALSE, indent = FALSE, percent.sign = TRUE,
  less.than.one = FALSE, remove.na = TRUE, label.stats = TRUE,
  digits = 1, p.digits = 4, ...
) {
  # Hard stops
  if (is.null(data) & !is.character(data) & !tibble::is_tibble(data)) stop('No data found. [check: data]')
  if (!is.null(by) & !is.character(by)) stop('Wrong type for \'by\'. Look at \'by = ...\'')

  # Setup environment
  list2env(list(...), envir = environment())
  data <- if (is.character(data)) eval(parse(text = data)) else data
  if (!tibble::is_tibble(data)) stop('Invalid data produced for row. Look at \'data = ...\'')

  # Check 'by' column
  if (!is.null(by)) {
    if (by %in% names(data) & (is.logical(data[[by]]) | is.factor(data[[by]]))) {
      if (is.logical(data[[by]])) data[[by]] <- as.factor(data[[by]])
    } else {
      by <- NULL
      warning('\'by\' column either missing from data or is not of logical/factor type. Ignoring.')
    }
  }

  # Tabulate row data
  if (!is.null(col)) {
    if(is.null(label)) label <- col
    if(indent) label <- paste0('   ', label)
    if (is.numeric(data[[col]]))
      table <- .row_numeric(
        label = label, col = col, data = data,
        by = by, parametric = parametric, percent.sign = percent.sign,
        less.than.one = less.than.one, label.stats = label.stats, remove.na = remove.na,
        digits = digits, p.digits = p.digits
      )
    else if (is.factor(data[[col]]))
      table <- .row_factor(
        label = label, col = col, data = data,
        by = by, parametric = parametric, percent.sign = percent.sign,
        label.stats = label.stats, remove.na = remove.na, digits = digits,
        p.digits = p.digits
      )
    else if (is.logical(data[[col]]))
      table <- .row_logical(
        label = label, col = col, data = data,
        by = by, parametric = parametric, inverse = inverse,
        percent.sign = percent.sign, label.stats = label.stats,
        remove.na = remove.na, digits = digits, p.digits = p.digits
      )
    else table <- NULL
  } else if (is.null(label))
    table <- .row_counts(
      label = ifelse(indent, '   n', 'n'), data = data, by = by,
      percent.sign = percent.sign, less.than.one = less.than.one, remove.na = remove.na,
      digits = digits
    )
  else table <- tibble::tibble(Variable = ifelse(indent, paste0('   ', label), label))

  if (is.null(table)) {
    # Gracefully handle unuseful data types
    warning(paste0('\'', col, '\' not of summarizable type! Returned unaltered tibble.'))
    if (!is.null(.table)) table <- .table
    else table <- tibble::tibble()
  } else {
    # Merge row data
    table <- .merge_rows(.table = .table, table = table, by = by, data = data)
    table <- .replace_na(table)
  }

  # Return table
  table
}


#' @title build_row_
#' @description A factory function to create a copy of build_row() with built in data and
#' pre-specified rules for row formatting.
#' @param by Optional. Character or Quosure. Name of factor or logical column to stratify by
#' or quosure code to be used by dplyr::mutate to create a grouping variable on the fly
#' (i.e. quo(pt_id %in% !!(data.followup %>% filter(lto_indexDeath) %>% .$pt_id))) in provided
#' tibbles. Either must be applicable to all provided tibbles!
#' @param data.default Optional. Character. Name of a provided tibble to use as default data source
#' if the \'data\' parameter is not specified in build_row().
#' @param remove.na Optional. Logical. Remove NA from denominator in frequency calculations.
#' Defaults to TRUE.
#' @param digits Optional. Integer. Number of digits to round numerics to. Defaults to 1.
#' @param p.digits Optional. Integer. Number of p-value digits to print. Note
#' that p-values are still rounded based on 'digits' parameter. Defaults to 4.
#' @param percent.sign Optional. Logical. Indicates percent sign should be printed for
#' frequencies. Defaults to TRUE.
#' @param less.than.one Optional. Logical. Indicates means/medians that round to 0 should be
#' printed as <1 (i.e. <1 [0-4]). Defaults to FALSE (0).
#' @param label.stats Optional. Logical. Whether to append the type of statistic (median, n, mean)
#' to the row's label. Defaults to TRUE.
#' @param parametric Optional. Logical. Indicates parametric testing should be used for comparisons
#' (Fisher's exact and Student's Unpaired T-Test). Defaults to FALSE (non-parametric; Chi-squared and
#' Wilcox Rank-sum).
#' @param inverse Optional. Logical. Indicates to summarize the FALSE/No data of a logical column
#' (i.e. 'Smoking Hx, yes' -> 'Smoking Hx, no'). Defaults to FALSE (Summarizes TRUE/Yes data).
#' @param indent Optional. Logical. Indent a variable labels. Defaults to FALSE.
#' @param ... Optional. Any other variables or tibbles you want to make available for use in
#' build_row(). I recommend naming each of these with a starting '.' to ensure they
#' do not conflict with other variables (i.e. build_row_(.index = data.index,
#' .radiology = data.radiology)). Note that this is optional as a tibble may be provided to
#' a resultant row function on the fly instead.
#' @return A custom build_row() function. See documentation for behavior.
#' @examples
#' library(dplyr)
#'
#' data_mtcars <- datasets::mtcars %>%
#'   as_tibble() %>%
#'   mutate_at(vars('vs', 'am'), as.logical) %>%
#'   mutate_at(vars('gear', 'carb', 'cyl'), as.factor)
#'
#' # Create instance of build_row() with custom defaults
#' row <- build_row_(
#'   by = 'am',
#'   percent.sign = FALSE,
#'   less.than.one = TRUE,
#'   label.stat = FALSE,
#'
#'   # Data (more than one allowed!)
#'   default.data = '.mtcars',
#'   .mtcars = data_mtcars,
#'   .mtcars_alt = data_mtcars
#' )
#'
#' row() %>% # Count row
#' row(label = 'Car Features') %>% # Row without data
#' row(label = 'Miles per gallon', col = 'mpg') %>%
#' row(col = 'cyl', data = '.mtcars %>% filter(mpg > 20)') %>% # subset of data
#' row(label = 'Horsepower', col = 'hp') %>%
#' row(label = 'Engine Shape', col = 'vs', percent.sign = TRUE, data = '.mtcars_alt')
#' @export
build_row_ <- function(
  by = NULL, data.default = NULL, digits = 1,
  percent.sign = FALSE, inverse = FALSE, indent = FALSE,
  less.than.one = FALSE, label.stats = TRUE, parametric = FALSE,
  p.digits = 4, remove.na = TRUE, ...
) {

  args <- list(...)
  tbl.names <- attributes(args)$names[sapply(X = args, FUN = tibble::is_tibble)]
  rm('...')

  # Verify \'by\' column in any provided tibbles
  if (!is.null(by)) {
    # Create by variable in any supplied tibbles
    if ((is.character(by) | rlang::is_quosure(by)) & length(tbl.names) > 0) {
      for (tbl.name in tbl.names) {
        if (rlang::is_quosure(by)) {
          args[[tbl.name]] <- dplyr::mutate(args[[tbl.name]], ".by" = as.factor(!! by))
        } else if (
          !('.by' %in% names(args[[tbl.name]])) &
          !is.factor(args[[tbl.name]][[by]]) &
          !is.logical(args[[tbl.name]][[by]])
        ) {
          warning('One or more provided tibbles missing the \'by\' column. Ignoring.')
          by <- NULL
        }
      }
      if (rlang::is_quosure(by)) by <- '.by'
      rm('tbl.name')
    } else {
      warning('Either no valid tibbles provided or \'by\' value incorrect. Look at \'...\' or \'by\'.')
      by <- NULL
    }
  }

  # Verify data.default
  if (!is.null(data.default)) {
    if (length(tbl.names) == 0 & !(data.default %in% tbl.names)) {
      warning('Cannot find default data tibble specified. Ignoring. Look at \'data.default\' and/or \'...\'.')
      data.default <- NULL
    }
  } else if (length(tbl.names) > 0) data.default <- sort(tbl.names)[1]

  # Mask environment var names
  for (name in names(environment())) {
    assign(x = paste0('.', name), value = eval(parse(text = name)))
    rm(list = name)
  }
  rm('name')

  # Print Defaults
  message('Pre-configured instance of build_row() created!')
  message('Running with empty \'col\' parameter will create a count row.')
  message('Parameter defaults:')
  message(paste0('  - data = ', ifelse(is.null(.data.default), 'NULL', .data.default)))
  message(paste0('  - by = ', ifelse(is.null(.by), 'NULL', .by)))
  message(paste0('  - parametric = ', .parametric))
  message(paste0('  - inverse = ', .inverse))
  message(paste0('  - indent = ', .indent))
  message(paste0('  - parametric = ', .parametric))
  message(paste0('  - less.than.one = ', .less.than.one))
  message(paste0('  - percent.sign = ', .percent.sign))
  message(paste0('  - label.stats = ', .label.stats))
  message(paste0('  - remove.na = ', .remove.na))
  message(paste0('  - digits = ', .digits))
  message(paste0('  - p.digits = ', .p.digits))

  # Return working function
  function(
    .table = NULL, label = NULL, col = NULL,
    by = .by, data = .data.default, parametric = .parametric,
    inverse = .inverse, indent = .indent, less.than.one = .less.than.one,
    percent.sign = .percent.sign, label.stats = .label.stats, remove.na = .remove.na,
    digits = .digits, p.digits = .p.digits
  ) {
    do.call(
      build_row,
      c(
        list(
          .table = .table, label = label,
          col = col, data = data,
          inverse = inverse, indent = indent,
          by = by, parametric = parametric,
          percent.sign = percent.sign, less.than.one = less.than.one,
          label.stats = label.stats, remove.na = remove.na,
          digits = digits, p.digits = p.digits
        ),
        .args
      )
    )
  }
}


#' @title build_footer
#' @description Creates a footer row specifying types of summary statistics. Does not
#' currently support "build_event" functions.
#' @param .table Optional. Tibble. A tibble to append row to.
#' @param cols Optional. Vector of characters. Column names summarized in table.
#' Used to determine appropriate types of summary stats. Defaults to considering
#' all columns in 'data'.
#' @param by Optional. Character. Name of factor or logical column table is stratified by.
#' @param data Required. Tibble. Contains data being summarized.
#' @param parametric Optional. Logical. Indicates parametric testing should be used for comparisons
#' (Fisher's exact and Student's Unpaired T-Test). Defaults to FALSE (non-parametric; Chi-squared and
#' Wilcox Rank-sum).
#' @return Data is returned in the form of a tibble containing the row(s).
#' @examples
#' library(dplyr)
#'
#' data_mtcars <- datasets::mtcars %>%
#'   as_tibble() %>%
#'   mutate_at(vars('vs', 'am'), as.logical) %>%
#'   mutate_at(vars('gear', 'carb', 'cyl'), as.factor)
#'
#' # Create footer row
#' build_footer(
#'   by = 'am',
#'   data = data_mtcars,
#'   parametric = FALSE
#' )
#' @export
build_footer <- function(.table = NULL, cols = NULL, by = NULL, data = NULL, parametric = FALSE) {

  # Hard stops
  if (!tibble::is_tibble(data)) stop('No usable data. [check: data]')

  # Autodetect columns
  if (is.null(cols)) cols <- names(data)

  # Check stats and tests
  stat_cat <- any(purrr::map_lgl(cols, ~ is.logical(data[[.x]]) | is.factor(data[[.x]])))
  stat_cont <- any(purrr::map_lgl(cols, ~ is.numeric(data[[.x]])))
  test_cat <- !is.null(by) & stat_cat
  test_cont <- !is.null(by) & stat_cont

  text <- paste(
    if (stat_cat | stat_cont) {
      paste0(
        'Statistics: ',
        paste(
          if (stat_cat) 'categorical, n(%)',
          if (stat_cont) paste('continuous,', if (parametric) 'mean\u00B1sd' else 'median[IQR]'),
          sep = '; '
        ),
        '.'
      )
    },
    if (test_cat | test_cont) {
      paste0(
        'Tests: ',
        paste(
          if (test_cat) paste('categorical,', if (parametric) 'Chi-squared test' else 'Fisher\'s exact test'),
          if (test_cont) paste('continuous,', if (parametric) 'Student\'s t-test' else 'Mann Whitney U & Kruskal-Wallis'),
          sep = '; '
        ),
        '.'
      )
    }
  )


  # Create table and merge with any given table
  table <- .merge_rows(.table = .table, table = tibble::tibble(Variable = text), by = by, data = data)
  table <- .replace_na(table)

  # Return result
  table
}
