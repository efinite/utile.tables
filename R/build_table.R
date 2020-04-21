#' @title Build summary tables
#' @examples
#' library(dplyr)
#'
#' data_mtcars <- datasets::mtcars %>%
#'   as_tibble() %>%
#'   mutate_at(vars('vs', 'am'), as.logical) %>%
#'   mutate_at(vars('gear', 'carb', 'cyl'), as.factor)
#'
#' # Summarize all columns by cylindars variable
#' data_mtcars %>% build_table(by = cyl, show.test = TRUE)
#'
#' # Summarize specific columns of data
#' data_mtcars %>% build_table(mpg, vs, carb)
#'
#' # Summarize columns using tidyselect helpers
#' data_mtcars %>% build_table(starts_with('c'), mpg, by = am)
#' @export
build_table <- function(...) { UseMethod('build_table') }


#' @export
build_table.default <- function (.object, ...) {
  stop('Object of class \'', class(.object), '\' no supported.')
}

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
  cols <- if (length(rlang::enquos(...)) > 0) {
    tidyselect::eval_select(rlang::expr(c(...)), data = .object)
  } else {
    rlang::set_names(1:length(names(.object)), names(.object))
  }

  # By variable selection and validation
  by <- if (length((.by <- rlang::enquo(.by)) == 1)) {
    tidyselect::eval_select(.by, data = .object)
  }

  if (length(by) > 0) {
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

