utils::globalVariables(c(
  'Variable', 'At Risk', 'Events',
  'HR', 'p'
))

#' @title build_event_table
#' @description Creates time-to-event models in an automated fashion and summarizes
#' them in a tibble.
#' @param fit Required. Formula, survival::survfit(), survival::coxph(). The formula
#' must contain a survival::Surv() object as first term. All terms must be present in data.
#' @param data Semi-optional. Tibble. Contains data for time-to-event model. Only required
#' if \'fit\' is a formula.
#' @param cols Optional. Character. Columns to use as predictors in time-to-event model.
#' Defaults to all usable columns in \'data\'.
#' @param skip Optional. Character. Names of columns to skip as part of predictor testing.
#' @param mv Optional. Logical. Indicates provided \'cols\' should be tested as part of one
#' multi-variate model. Defaults to FALSE (univariate; seperate models).
#' @param percent.sign Optional. Logical. Indicates percent sign should be printed
#' for frequencies. Defaults to TRUE.
#' @param digits Optional. Integer. Number of digits to round numerics to. Defaults to 1.
#' @param p.digits Optional. Integer. Number of digits to print for p-values. Note that p-values are
#' still rounded based on \'digits\' parameter. Defaults to 4.
#' @return Data is returned in the form of a tibble containing a row for each parameter.
#' @examples
#' library(survival)
#' library(dplyr)
#'
#' data_lung <- lung %>%
#'   as_tibble() %>%
#'   mutate_at(vars(inst, status, sex), as.factor) %>%
#'   mutate(status = case_when(status == 1 ~ 0, status == 2 ~ 1))
#'
#' # Automatically model each parameter
#' build_event_table(Surv(time, status) ~ 1, skip = 'inst', data = data_lung)
#'
#' # Automatically model all parameters together
#' build_event_table(Surv(time, status) ~ 1, skip = 'inst', mv = TRUE, data = data_lung)
#' @export
build_event_table <- function(fit, data, cols, skip, mv, percent.sign, digits, p.digits) {
  UseMethod('build_event_table')
}


.build_event_table <- function(
  fit = NULL, data = NULL, cols = NULL,
  skip = NULL, mv = FALSE, percent.sign = TRUE,
  digits = 1, p.digits = 4
) {

  fit_vars <- all.vars(fit)

  # Hard stops
  if (!tibble::is_tibble(data) & !is.data.frame(data))
    stop('Missing formula data. [check: data]')
  if (length(fit_vars) != length(intersect(fit_vars, names(data))))
    stop('Terms in formula not present in data. [check: fit, data]')
  if (as.character(fit[[2]])[1] != 'Surv')
    stop('First term must be survival::Surv() object. [check: fit]')

  # Select columns
  cols <- if (is.character(cols)) cols[cols %in% names(data)] else names(data)
  if (is.character(skip)) cols <- setdiff(cols, skip)
  cols <- cols[!(cols %in% fit_vars)]

  # Build table
  table <- NULL
  for (col in cols) {
    if (!is.numeric(data[[col]]) & !is.factor(data[[col]]) & !is.logical(data[[col]]))
      message(paste0('Skipping:    ', col, ' (unusable type)'))
    else if (all(is.na(data[[col]])))
      message(paste0('Skipping:    ', col, ' (all NA)'))
    else if (is.factor(data[[col]]) & length(levels(data[[col]])) < 2)
      message(paste0('Skipping:    ', col, ' (factor <2 lvls)'))
    else {
      table <- build_event_row(
        .table = table,
        col = col,
        fit = survival::coxph(
          stats::as.formula(
            paste0(
              c(
                deparse(fit), # important: resets environment
                if (mv) paste(cols, collapse = ' + ')
                else col
              ),
              collapse = ' + '
            )
          ),
          data = data,
        ),
        percent.sign = percent.sign,
        digits = digits,
        p.digits = p.digits
      )
    }
  }

  # Return table
  table
}


#' @export
build_event_table.formula <- .build_event_table


#' @export
build_event_table.coxph <- function(
  fit = NULL, data = NULL, cols = NULL,
  skip = NULL, mv = FALSE, percent.sign = TRUE,
  digits = 1, p.digits = 4
) {
  # Seperate data and formula from survival object
  if (is.null(data)) data <- eval(fit$call$data)
  fit <- stats::formula(fit)

  .build_event_table(
    fit = fit, data = data,cols = cols,
    skip = skip, mv = mv, percent.sign = percent.sign,
    digits = digits, p.digits = p.digits
  )
}


#' @export
build_event_table.survfit <- build_event_table.coxph
