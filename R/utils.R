# Make NA's human readable
.replace_na <- function (x) replace(x, is.na(x), '')

# Make factor NA's explicit
.explicit_na <- function (x, y = 'Missing') {
  x_na <- is.na(x)
  lvl_na <- is.na(levels(x))
  if(any(lvl_na)) levels(x)[lvl_na] <- y
  if (any(x_na)) {
    levels(x) <- c(levels(x), y)
    x[x_na] <- y
  }
  x
}

# Get levels of factor, ordinal, or logical
.get_levels <- function (x) {
  lvls <-
    if (inherits(x, c('factor', 'ordered'))) levels(x)
    else if (inherits(x, 'logical')) {
      c(TRUE, FALSE)[c(TRUE, FALSE) %in% unique(x)]
    } else character()
  rlang::set_names(lvls)
}

# Create assignments for a model
.create_assigns <- function (x, y) {
  rlang::set_names(x = purrr::imap(x, ~ which(.y - 1 == y)), nm = x)
}

# Clean data and formula environment
.refit_model <- function (x, formula, na.rm = FALSE) {

  # Get model call
  call <- x$call

  # Reset formula environment
  call$formula <- stats::as.formula(
    if (!missing(formula)) formula
    else call$formula
  )

  # Remove NA's from data
  if (na.rm) {
    call$data <- stats::na.omit(eval(call$data)[all.vars(call$formula)])
  }

  # Refit and return
  eval(call, parent.frame())

}
