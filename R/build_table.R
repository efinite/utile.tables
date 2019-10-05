#' @title build_table
#' @description A function for summarizing columns of data. Can work in an automated fashion
#' or with manually specified options. It is essentially a wrapper for build_row().
#' @param data Required. Tibble. Contains data to be summarized.
#' @param by Optional. Character. Name of factor or logical column to stratify
#' summaries by.
#' @param cols Optional. Character. Contains character names of columns to summarize. Defaults
#' to all columns.
#' @param skip Optional. Character. Names of columns to skip as part of predictor testing.
#' @param remove.na Optional. Logical. Remove NA from denominator in frequency calculations.
#' Defaults to TRUE.
#' @param digits Optional. Integer. Number of digits to round numerics to. Defaults to 1.
#' @param p.digits Optional. Integer. Number of p-value digits to print. Note
#' that p-values are still rounded based on \'digits\' parameter. Defaults to 4.
#' @param percent.sign Optional. Logical. Indicates percent sign should be printed for
#' frequencies. Defaults to TRUE.
#' @param less.than.one Optional. Logical indicating whether means/medians that round to 0
#' should be printed as <1 (i.e. <1 [0-4]). Defaults to printing the 0.
#' @param parametric Optional. Logical. Indicates parametric testing should be used for comparisons
#' (Fisher's exact and Student's Unpaired T-Test). Defaults to FALSE (non-parametric; Chi-squared and
#' Wilcox Rank-sum).
#' @param inverse Optional. Logical. Indicates to summarize the FALSE/No data of logical columns
#' (i.e. 'Smoking Hx, yes' -> 'Smoking Hx, no'). Defaults to FALSE (Summarizes TRUE/Yes data).
#' @param indent Optional. Logical. Indent variable labels. Defaults to FALSE.
#' @param footer.stats Optional. Logical. Most stats summary into a footer row. Removes the stat
#' type from row labels. Defaults to FALSE.
#' @return Data is returned in the form of a tibble containing the row(s).
#' @examples
#' library(dplyr)
#'
#' data_mtcars <- datasets::mtcars %>%
#'   as_tibble() %>%
#'   mutate_at(vars('vs', 'am'), as.logical) %>%
#'   mutate_at(vars('gear', 'carb', 'cyl'), as.factor)
#'
#' build_table(
#'   data = data_mtcars,
#'   by = 'vs',
#'   cols = c(
#'     'gear',
#'     'mpg',
#'     'carb',
#'     'am',
#'     'hp'
#'   ),
#'   percent.sign = FALSE,
#'   less.than.one = TRUE,
#'   footer.stats = TRUE
#' )
#' @export
build_table <- function(
  data = NULL, by = NULL, cols = NULL,
  skip = NULL, digits = 1, percent.sign = FALSE,
  less.than.one = FALSE, inverse = FALSE, indent = FALSE,
  parametric = FALSE, footer.stats = FALSE, p.digits = 4,
  remove.na = TRUE
) {

  # Hard stops
  if (!tibble::is_tibble(data)) stop('No usable tibble found. [check: data]')

  # Check 'by' column
  if (!is.null(by)) {
    if (by %in% names(data) & (is.logical(data[[by]]) | is.factor(data[[by]]))) {
      if (is.logical(data[[by]])) data[[by]] <- as.factor(data[[by]])
    } else {
      by <- NULL
      warning('Unusable \'by\' column. Ignoring. [check: by, data]')
    }
  }

  # Detect column names and validate
  cols <- if (is.character(cols)) cols[cols %in% names(data)] else names(data)
  if (is.character(skip)) cols <- setdiff(cols, skip)

  # Create n= row
  table <- build_row(
    data = data, by = by, percent.sign = percent.sign,
    less.than.one = less.than.one, remove.na = remove.na, digits = digits,
    p.digits = p.digits
  )

  # Create row for each column
  for (col in cols)
    table <- build_row(
      .table = table, col = col, data = data,
      inverse = inverse, indent = indent, by = by,
      parametric = parametric, percent.sign = percent.sign, less.than.one = less.than.one,
      label.stats = !footer.stats, remove.na = remove.na, digits = digits,
      p.digits = p.digits
    )

  # Add footer stats row, if applicable
  if (footer.stats)
    table <- build_footer(.table = table, cols = cols, by = by, data = data, parametric = parametric)

  # Return data
  table
}
