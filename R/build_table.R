#' @title Build summary tables
#' @param .object An object of a supported class. See S3 methods below
#' @param ... Arguments passed to the appropriate S3 method.
#' @return A \code{\link[tibble:tibble]{tibble::tibble()}} summarizing the
#' provided object.
#' @seealso \code{\link{build_table.data.frame}}
#' @export
build_table <- function(.object, ...) { UseMethod('build_table') }


#' @export
build_table.default <- function (.object, ...) {
  stop('Object of class \'', class(.object), '\' no supported.')
}

#' @rdname build_table.data.frame
#' @title Build summary tables from data.frame objects
#' @param .object A data.frame.
#' @param ... One or more unquoted expressions separated by commas representing
#' columns in the data.frame. May be specified using
#' \code{\link[tidyselect:select_helpers]{tidyselect helpers}}.
#' @param .by An unquoted expression representing a column to stratify summaries
#' by.
#' @param .inverse A logical. For logical data, report the frequency of FALSE
#' values instead of the TRUE.
#' @param .append.stat A logical. Append the summary statistic to the column
#' label.
#' @param .parametric A logical. Use parametric tests as opposed to
#' non-parametric.
#' @param .show.missing A logical. Append a column listing the frequencies of
#' missing data for each row.
#' @param .show.test A logical. Append a column containing the test each p-value
#' was derived from.
#' @param .na.rm A logical. Ignore NA values when calculating the frequencies
#' for logical and factor data types.
#' @param .percent.sign A logical. Paste a percent symbol after all reported
#' frequencies.
#' @param .digits An integer. The number of digits to round numbers to.
#' @param .p.digits An integer. The number of p-value digits to report. Note
#' that the p-value still rounded to the number of digits specified in
#' \code{.digits}.
#' @examples
#' library(dplyr)
#'
#' data_mtcars <- datasets::mtcars %>%
#'   as_tibble() %>%
#'   mutate_at(vars('vs', 'am'), as.logical) %>%
#'   mutate_at(vars('gear', 'carb', 'cyl'), as.factor)
#'
#' # Summarize all columns by cylindars variable
#' data_mtcars %>% build_table(.by = cyl, .show.test = TRUE)
#'
#' # Summarize specific columns of data
#' data_mtcars %>% build_table(mpg, vs, carb)
#'
#' # Summarize columns using tidyselect helpers
#' data_mtcars %>% build_table(starts_with('c'), mpg, .by = am)
#' @export
build_table.data.frame <- function(
  .object,
  ...,
  .by,
  .inverse = FALSE,
  .append.stat = TRUE,
  .parametric = FALSE,
  .show.missing = FALSE,
  .show.test = FALSE,
  .na.rm = TRUE,
  .percent.sign = TRUE,
  .digits = 1,
  .p.digits = 4
) {

  # Column selection
  cols <- if (length(rlang::enexprs(...)) > 0) {
    tidyselect::eval_select(rlang::expr(c(...)), data = .object)
  } else {
    rlang::set_names(1:length(names(.object)), names(.object))
  }

  # By variable selection and validation
  by <- if (!missing(.by) & length((.by <- rlang::enexpr(.by)) == 1)) {
    tidyselect::eval_select(.by, data = .object)
  }

  print(length(by))

  if (length(by) > 0) {
    print('hello!')
    if (is.logical(.object[[by]]) | is.factor(.object[[by]])) {

      # Cast logicals to factors
      if (is.logical(.object[[by]])) .object[[by]] <- as.factor(.object[[by]])

      # Explicit NA's
      .object[[by]] <- .explicit_na(.object[[by]])

      # Prevent summarization
      cols <- cols[!(cols %in% by)]

    } else by <- integer()
  }

  # Pre-specify row configuration
  build_row_ <- function (x, ...) {
    build_row(
      x = x,
      y = if (length(by) == 1) .object[[by]],
      ...,
      inverse = .inverse,
      append.stat = .append.stat,
      parametric = .parametric,
      show.missing = .show.missing,
      show.test = .show.test,
      na.rm = .na.rm,
      percent.sign = .percent.sign,
      digits = .digits,
      p.digits = .p.digits
    )
  }

  # Create table
  table <- dplyr::bind_rows(
    build_row_(x = .object),
    purrr::imap_dfr(cols, ~ build_row_(x = .object[[.x]], label = .y))
  )

  # Replace return table
  .replace_na(table)

}

