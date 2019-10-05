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


#' @title build_event_row_
#' @description A factory for creating a copy of build_event_row() with built in
#' data, fit, and customized defaults.
#' @param fit Required. Formula, survival::survfit(), survival::coxph(). The formula
#' must contain a survival::Surv() object as first term. All terms must be present in data.
#' @param data Semi-optional. Tibble. Contains data for time-to-event model. Only required
#' if \'fit\' is a formula.
#' @param percent.sign Optional. Logical. Indicates percent sign should be printed
#' for frequencies. Defaults to TRUE.
#' @param digits Optional. Integer. Number of digits to round numerics to. Defaults to 1.
#' @param p.digits Optional. Integer. Number of digits to print for p-values. Note that p-values are
#' still rounded based on \'digits\' parameter. Defaults to 4.
#' @param indent Optional. Logical. Indent a variable labels. Defaults to FALSE.
#' @return A custom build_event_row() function. See related documentation for behavior.
#' @examples
#' library(survival)
#' library(dplyr)
#'
#' data_lung <- lung %>%
#'   as_tibble() %>%
#'   mutate_at(vars(inst, status, sex), as.factor) %>%
#'   mutate(status = case_when(status == 1 ~ 0, status == 2 ~ 1))
#'
#' row <- build_event_row_(
#'   Surv(time, status) ~ 1,
#'   data = data_lung,
#'   digits = 2
#' )
#'
#' row(label = 'Age, years', col = 'age') %>%
#' row(label = 'Sex', col = 'sex') %>%
#' row(label = 'Institution', col = 'inst')
#' @export
build_event_row_ <- function(fit, data, percent.sign, digits, p.digits, indent) {
  UseMethod('build_event_row_')
}


.build_event_row_ <- function(
  fit = NULL, data = NULL, percent.sign = TRUE,
  digits = 1, p.digits = 4, indent = FALSE
) {
  # Extract formula variables
  fit_vars <- all.vars(fit)

  # Hard stops
  if (!tibble::is_tibble(data) & !is.data.frame(data))
    stop('Missing formula data. [check: data]')
  if (length(fit_vars) != length(intersect(fit_vars, names(data))))
    stop('Terms in formula not present in \'data\'. [check: fit, data]')
  if (as.character(fit[[2]])[1] != 'Surv')
    stop('First term must be survival::Surv() object. [check: fit]')

  # Mask environment variable names
  for (name in names(environment())) {
    assign(x = paste0('.', name), value = eval(parse(text = name)))
    rm(list = name)
  }
  rm('name')

  # Print Defaults
  message('Pre-configured instance of build_event_row() created!')
  message('Parameter defaults:')
  message(paste0('  - fit = ', .fit))
  message('  - data = data')
  message(paste0('  - percent.sign = ', .percent.sign))
  message(paste0('  - digits = ', .digits))
  message(paste0('  - p.digits = ', .p.digits))
  message(paste0('  - indent = ', .indent))

  # Return working function
  function(
    .table = NULL, label = NULL, col = NULL,
    fit = .fit, data = .data, percent.sign = .percent.sign,
    digits = .digits, p.digits = .p.digits, indent = .indent
  ) {
    table <- .table

    if (!is.null(col)) {
      if (!is.numeric(data[[col]]) & !is.factor(data[[col]]) & !is.logical(data[[col]]))
        message(paste0('Skipping:    ', col, ' (unusable type)'))
      else if (all(is.na(data[[col]])))
        message(paste0('Skipping:    ', col, ' (all NA)'))
      else if (is.factor(data[[col]]) & length(levels(data[[col]])) < 2)
        message(paste0('Skipping:    ', col, ' (factor <2 lvls)'))
      else
        table <- do.call(
          build_event_row,
          c(
            list(
              .table = .table, label = label, col = col,
              fit = survival::coxph(
                stats::as.formula(
                  paste0(
                    deparse(fit), # important: resets environment
                    ' + ',
                    col
                  )
                ),
                data = data,
              ),
              percent.sign = percent.sign, digits = digits,
              p.digits = p.digits, indent = indent
            )
          )
        )
    } else table <- do.call(build_event_row, list(.table = .table, label = label, indent = indent))

    # Return table
    table
  }
}


#' @export
build_event_row_.formula <- .build_event_row_


#' @export
build_event_row_.coxph <- function(
  fit = NULL, data = NULL, percent.sign = TRUE,
  digits = 1, p.digits = 4, indent = indent
) {
  # Extract data and formula from survival object
  if (is.null(data)) data <- eval(fit$call$data)
  fit <- stats::formula(fit)

  .build_event_row_(
    fit = fit, data = data, percent.sign = percent.sign,
    digits = digits, p.digits = p.digits, indent = indent
  )
}


#' @export
build_event_row_.survfit <- build_event_row_.coxph
