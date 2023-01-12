#' @title Build models
#' @description Models specified terms in model data against an existing model
#' and returns a clean, human readable table of summarizing the effects and
#' statistics for the newly generated model. This function is meant to simplify
#' fitting a large number of variables against a set of time-to-event data.
#' @param .object An object of a supported class. See S3 methods below.
#' @param ... Arguments passed to the appropriate S3 method.
#' @return An object of class \code{tbl_df} (tibble) summarizing the provided
#' object.
#' @seealso \code{\link{build_model.coxph}}
#' @export
build_model <- function (.object, ...) { UseMethod('build_model') }


#' @export
build_model.default <- function (.object, ...) {
  stop('Object of class \'', class(.object), '\' no supported.')
}


#' @rdname build_model.coxph
#' @title Build Cox PH models
#' @description Models specified terms in model data against an existing model
#' and returns a clean, human readable table of summarizing the effects and
#' statistics for the newly generated model. This functions greatly simplifies
#' fitting a large number of variables against a set of time-to-event data.
#' @param .object An object of class \code{\link[survival]{coxph}}.
#' @param ... One or more unquoted expressions separated by commas representing
#' columns in the model data.frame. May be specified using
#' \code{\link[tidyselect:select_helpers]{tidyselect helpers}}.
#' @param .mv A logical. Fit all terms into a single multivariable model. If left
#' FALSE, all terms are fit in their own univariate models.
#' @param .test A character. The name of a \code{\link[stats:add1]{stats::drop1}}
#' test to use with the model.
#' @param .col.test A logical. Append a columns for the test and accompanying
#' statistic used to derive the p-value.
#' @param .level A double. The confidence level required.
#' @param .stat.pct.sign A logical. Paste a percent symbol after all reported
#' frequencies.
#' @param .digits An integer. The number of digits to round numbers to.
#' @param .p.digits An integer. The number of p-value digits to report. Note
#' that the p-value still rounded to the number of digits specified in
#' \code{.digits}.
#' @return An object of class data.frame summarizing the provided object. If the
#' \code{tibble} package has been installed, a tibble will be returned.
#' @seealso \code{\link{build_model}}
#' @examples
#' library(survival)
#' library(dplyr)
#'
#' data_lung <- lung |>
#'   mutate_at(vars(inst, status, sex), as.factor) |>
#'   mutate(status = case_when(status == 1 ~ 0, status == 2 ~ 1))
#'
#' fit <- coxph(Surv(time, status) ~ 1, data = data_lung)
#'
#' # Create a univariate model for each variable
#' fit |> build_model(sex, age)
#' @export
build_model.coxph <- function (
  .object,
  ...,
  .mv = FALSE,
  .test = c('LRT', 'Wald'),
  .col.test = FALSE,
  .level = 0.95,
  .stat.pct.sign = TRUE,
  .digits = 1,
  .p.digits = 4
) {

  # Reconcile .test argument
  .test <- match.arg(.test)

  # Retrieve data.frame from call
  data <- eval(.object$call$data)
  base_formula <- stats::formula(.object)

  # Column selection
  terms <- if (rlang::dots_n(...) > 0) {
    tidyselect::eval_select(expr = rlang::expr(c(...)), data = data)
  } else {
    rlang::set_names(x = 1:length(names(data)), nm = names(data))
  }

  # Ignore unusable terms
  terms <- terms[
    !(names(terms) %in% all.vars(base_formula)) &
    purrr::imap_lgl(
      terms,
      ~ {
        !((!is.numeric(data[[.x]]) & !is.factor(data[[.x]]) & !is.logical(data[[.x]])) |
        all(is.na(data[[.x]])) |
        (is.factor(data[[.x]]) & length(levels(data[[.x]])) < 2))
      }
    )
  ]

  # Convert formula to character
  base_formula <- deparse(base_formula)

  # build_table factory with pre-specified defaults
  build_table_ <- function (...) {
    build_table(
      ...,
      .test = .test,
      .col.test = .col.test,
      .level = .level,
      .stat.pct.sign = .stat.pct.sign,
      .digits = .digits,
      .p.digits = .p.digits
    )
  }

  # Refit model and build summary table
  if (!.mv) {

    # Univariable modelling
    purrr::list_rbind(
      purrr::imap(
        terms,
          ~ {
          build_table_(
            .object = .refit_model(
              x = .object,
              formula = paste(base_formula, .y, sep = ' + ')
            ),
            !! .y
          )
        }
      )
    )

  } else {

    # Multivariable modelling
    build_table_(
      .object = .refit_model(
        x = .object,
        formula = paste(
          base_formula,
          paste(names(terms), collapse = ' + '),
          sep = ' + '
        ),
        na.rm = TRUE
      ),
      !!! names(terms)
    )

  }

}
