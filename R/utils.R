utils::globalVariables(c(
  'variable', 'level', 'subjects',
  'events', 'estimate', 'conf.lower',
  'conf.upper', 'p', ':='
))

# Create coxph summary row
.row_coxph <- function (label, col, fit, percent.sign, digits, p.digits) {

  # Hard stop
  if (is.null(fit) | class(fit) != 'coxph') stop('Missing valid coxph object. [check: fit]')

  model_table <- utile.tools::tabulate_coef(fit = fit, format = FALSE)

  # Identify reference rows
  has_levels <- 'level' %in% names(model_table)
  if (has_levels) ref_rows <- !is.na(model_table$level) & is.na(model_table$estimate)

  # Transmute model_table
  res <- dplyr::bind_cols(

    # Variable
    Variable = purrr::map2_chr(
      model_table$variable, if (has_levels) model_table$level else NA,
      ~ if (!is.na(.y)) {
        if (nrow(model_table[model_table$variable == .x,]) == 1)
          paste(if (!is.null(label)) label else .x, .y, sep = ', ')
        else paste0('   ', .y)
      } else if (!is.null(label)) label
      else .x
    ),

    # Subjects
    Subjects = as.character(model_table$subjects),

    # Events
    Events = utile.tools::paste_freq(
      x = model_table$events, y = model_table$subjects,
      percent.sign = percent.sign, digits = digits
    ),

    # HR
    `HR [95%CI]` = dplyr::if_else(
      !is.na(model_table$estimate),
      paste(
        round(model_table$estimate, digits = digits),
        dplyr::if_else(
          !is.na(model_table$conf.low) & !is.na(model_table$conf.high),
          paste0('[', round(model_table$conf.low, digits = digits), '-', round(model_table$conf.high, digits = digits), ']'),
          ''
        )
      ),
      as.character(NA)
    ),

    # Format p-value
    p = format.pval(pv = model_table$p, digits = digits, eps = 0.0001, nsmall = p.digits, scientific = F, na.form = '')
  )

  # Set reference row placeholders
  if (has_levels) res[ref_rows, 'HR [95%CI]'] <- 'Reference'

  # Return results
  res

}


# Merge rows
.merge_rows <- function(.table, table, by, data) {

  # Merge tables
  if (!is.null(.table)) table <- dplyr::bind_rows(.table, table)

  # Detect main columns
  cols <- intersect(c('Variable', 'Overall'), names(table))

  # Detect level columns
  if (!is.null(by)) {
    levels.by <- sort(levels(data[[by]]))
    cols <- c(
      cols,
      intersect(levels.by, names(table)),
      intersect(paste0('Missing: ', levels.by), names(table))
    )
  }

  # Detect miscellaneous columns
  cols <- c(cols, intersect(c('Missing', 'Probability', 'Test'), names(table)))
  table <- dplyr::select_at(
    .tbl = table,
    .vars = dplyr::vars(
      cols,
      dplyr::everything() # Preserve unmatched columns
    )
  )

  # Return table
  table
}


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
