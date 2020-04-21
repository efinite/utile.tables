utils::globalVariables(c('.mv', '.fit', '.data'))

#' @title build_event_row
#' @description Creates a tibble row summarizing a predictor (column) in a given time-to-event model.
#' @param .table Optional. Tibble. A tibble for row to be appended.
#' @param label Optional. Character. Row name to print in the table. Defaults to value of \'col\' parameter.
#' @param col Required. Character. Name of column used as a parameter in the tiem-to-event function.
#' @param fit Required. survival::coxph().
#' @param percent.sign Optional. Logical. Indicates percent sign should be printed
#' for frequencies. Defaults to TRUE.
#' @param digits Optional. Integer. Number of digits to round numerics to. Defaults to 1.
#' @param p.digits Optional. Integer. Number of digits to print for p-value. Note that p-values are still
#' rounded based on the \'digits\' parameter. Defaults to 4.
#' @param indent Optional. Logical. Indent a variable labels. Defaults to FALSE.
#' @return Data is returned in the form of a tibble containing a row for the specified parameter.
#' @examples
#' library(survival)
#' library(dplyr)
#'
#' data_lung <- lung %>%
#'   as_tibble() %>%
#'   mutate_at(vars(inst, status, sex), as.factor) %>%
#'   mutate(status = case_when(status == 1 ~ 0, status == 2 ~ 1))
#'
#' # Stand-alone row
#' build_event_row(
#'    label = 'Meal calories',
#'    col = 'meal.cal',
#'    fit = coxph(Surv(time, status) ~ meal.cal, data = data_lung)
#' )
#'
#' # Build a table row-by-row
#' build_event_row(
#'   label = 'Age, years',
#'   col = 'age',
#'   fit = coxph(Surv(time, status) ~ age, data = data_lung)
#' ) %>%
#' build_event_row(
#'   label = 'Sex',
#'   col = 'sex',
#'   fit = coxph(Surv(time, status) ~ sex, data = data_lung)
#' ) %>%
#' build_event_row(
#'   label = 'Institution',
#'   col = 'inst',
#'   fit = coxph(Surv(time, status) ~ inst, data = data_lung)
#' )
#' @export
build_event_row <- function(
  .table = NULL, label = NULL, col = NULL,
  fit = NULL, percent.sign = TRUE, digits = 1,
  p.digits = 4, indent = FALSE
) {
  # Tabulate row data
  if (!is.null(col)) {
    if (is.null(label)) label <- col
    if (indent) label <- paste0('   ', label)
    table <- .row_coxph(
      label = label, col = col, fit = fit,
      percent.sign = percent.sign, digits = digits, p.digits = p.digits
    )
  } else if (!is.null(label)) table <- tibble::tribble(~Variable, ifelse(indent, paste0('   ', label), label))
  else table <- NULL

  # Merge with given data
  if (is.null(table)) {
    # Gracefully handle unuseful data types
    if (!is.null(.table)) table <- .table
    else table <- tibble::tibble()
  } else {
    # Merge row data
    table <- dplyr::bind_rows(.table, table)
    table <- .replace_na(table)
  }

  # Return table
  table
}
