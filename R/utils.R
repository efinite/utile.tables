# Make NA's human readable
.replace_na <- function(x) replace(x, is.na(x), '')


# Make factor NA's explicit
.explicit_na <- function(x) {
  x_na <- is.na(x)
  if (any(x_na)) {
    levels(x) <- c(levels(x), '(NA)')
    x[x_na] <- '(NA)'
  }
  x
}


# Create assignments for a model
.create_assigns <- function (x, y) {
  rlang::set_names(x = purrr::imap(x, ~ which(.y - 1 == y)), nm = x)
}

# Clean data and formula environment
.refit_model <- function(x, formula, na.rm = FALSE) {

  # Get model call
  call <- stats::getCall(x)

  # Reset formula environment
  call$formula <- stats::as.formula(
    deparse(if (!missing(formula)) formula else stats::formula(x))
  )

  # Remove NA's from data
  if (na.rm) {
    call$data <- stats::na.omit(
      get(
        x = as.character(call$data),
        envir = parent.frame()
      )[all.vars(call$formula)]
    )
  }

  # Refit and return
  eval(call, parent.frame())
}
