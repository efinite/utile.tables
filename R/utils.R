utils::globalVariables(c(
  'variable', 'level', 'subjects',
  'events', 'estimate', 'conf.lower',
  'conf.upper', 'p', ':='
))

# Create a count summary row
.row_counts <- function(
  label, data, by,
  percent.sign, less.than.one, remove.na,
  digits
) {

  # Counts w/ strata
  if (!is.null(by))
    dplyr::bind_cols(
      Variable = label,
      Overall = as.character(nrow(data)), # Overall

      purrr::map_dfc(
        sort(levels(data[[by]])), # Levels to map
        ~ tibble::tibble(
          !!.x := utile.tools::paste_freq(
            count = dplyr::filter(data, !! rlang::sym(by) == .x), # Rows in level
            total = # Total rows
              if (remove.na) dplyr::filter(.data = data, !is.na(!! rlang::sym(by)))
              else data,
            percent.sign = percent.sign,
            digits = digits
          )
        )
      )
    )

  # Counts w/o stratification
  else tibble::tibble(Variable = label, Overall = as.character(nrow(data)))

}


# Create a numeric summary row
.row_numeric <- function(
  label, col, data,
  by, parametric, percent.sign,
  less.than.one, label.stats, remove.na,
  digits, p.digits
) {

  # Append statistic to label, if appropriate
  if (label.stats)
    label <- if (!parametric) paste0(label, ', median[IQR]') else paste0(label, ', mean\u00B1SD')

  # Overall summary stat
  overall_stat <-
    if (!parametric)
      utile.tools::paste_median(
        col = data[[col]],
        less.than.one = less.than.one,
        digits = digits
      )
    else
      utile.tools::paste_mean(
        col = data[[col]],
        less.than.one = less.than.one,
        digits = digits
      )

  # Summarize strata
  if (!is.null(by))
    dplyr::bind_cols(
      Variable = label,
      Overall = overall_stat,

      purrr::map_dfc(
        sort(levels(data[[by]])), # Strata
        function (x) {
          data_by <- dplyr::filter(data, !! rlang::sym(by) == x) # Stratum subset
          tibble::tibble(

            # Stratum statistic
            !!x :=
              if (!parametric)
                utile.tools::paste_median(
                  col = data_by[[col]],
                  less.than.one = less.than.one,
                  digits = digits
                )
              else
                utile.tools::paste_mean(
                  data_by[[col]],
                  less.than.one = less.than.one,
                  digits = digits
                ),

            # Stratum missing count
            !!paste0('Missing: ', x) := utile.tools::paste_freq(
                count = dplyr::filter(.data = data_by, is.na(!! rlang::sym(col))),
                total = data_by,
                percent.sign = percent.sign,
                digits = digits
              )
          )
        }
      ),

      # Strata test
      Probability = utile.tools::test_numeric(
        col = col, by = by, parametric = parametric,
        data = data, digits = digits, p.digits = p.digits
      ),

      # Strata test labels
      if (label.stats) tibble::tibble(
        Test = if (!parametric) 'Wilcox {NP}' else 'Student\'s {P}'
      )
    )

  # Overall summary w/o strata
  else
    dplyr::bind_cols(
      Variable = label,
      Overall = overall_stat,
      Missing = utile.tools::paste_freq(
        count = dplyr::filter(data, is.na(!! rlang::sym(col))),
        total = data,
        percent.sign = percent.sign,
        digits = digits
      )
    )

}


# Create a factor summary row
.row_factor <- function(
  label, col,
  data, by,
  parametric, percent.sign,
  label.stats, remove.na,
  digits, p.digits
) {

  # Append statistic to label, if appropriate
  if (label.stats) label <- paste0(label, ', n(%)')

  dplyr::bind_rows(

    # Factor variable row
    dplyr::bind_cols(
      Variable = label,

      # Count & test strata
      if (!is.null(by))
        dplyr::bind_cols(

          # Strata missing
          purrr::map_dfc(
            sort(levels(data[[by]])), # Strata
            function (x) {
              data_by <- dplyr::filter(data, !! rlang::sym(by) == x) # Stratum subset
              tibble::tibble(
                !!paste0('Missing: ', x) := utile.tools::paste_freq(
                  count = dplyr::filter(.data = data_by, is.na(!! rlang::sym(col))),
                  total = data_by,
                  percent.sign = percent.sign,
                  digits = digits
                )
              )
            }
          ),

          # Strata tests
          tibble::tibble(
            Probability = utile.tools::test_factor(
              col = col, by = by, parametric = parametric,
              data = data, digits = digits, p.digits = p.digits
            )
          ),

          # Test labels
          if (label.stats) {
            tibble::tibble(Test = if (!parametric) 'Chisq {NP}' else 'Fisher\'s {P}')
          }
        )

      # Overall missing
      else
        tibble::tibble(
          Missing = utile.tools::paste_freq(
            count = dplyr::filter(data, is.na(!! rlang::sym(col))),
            total = data,
            percent.sign = percent.sign,
            digits = digits
          )
        )

    ),

    # Factor level rows
    purrr::map_dfr(
      sort(levels(data[[col]])), # Level names
      function (x) {
        data_level <- dplyr::filter(data, !! rlang::sym(col) == x)
        dplyr::bind_cols(

          # Overall counts
          Variable = paste0('  ', x),
          Overall = utile.tools::paste_freq(
            count = data_level,
            total =
              if (remove.na)
                dplyr::filter(.data = data, !is.na(!! rlang::sym(col)))
              else data,
            percent.sign = percent.sign,
            digits = digits
          ),

          # Strata counts
          if (!is.null(by)) {
            purrr::map_dfc(
              sort(levels(data[[by]])),
              function (y) {
                tibble::tibble(
                  !!y := utile.tools::paste_freq(
                    count = dplyr::filter(data_level, !! rlang::sym(by) == y),
                    total =
                      if (remove.na)
                        dplyr::filter(data, !! rlang::sym(by) == y & !is.na(!! rlang::sym(col)))
                    else dplyr::filter(data, !! rlang::sym(by) == y),
                    percent.sign = percent.sign,
                    digits = digits
                  )
                )
              }
            )
          }

        )
      }
    )

  )
}


# Create a logical summary row
.row_logical <- function(
  label, col,
  data, by,
  parametric, inverse,
  percent.sign, label.stats,
  remove.na, digits,
  p.digits
) {

  # Append statistic to label, if appropriate
  label <- if (!inverse) paste0(label, ', yes') else paste0(label, ', no')
  if (label.stats) label <- paste0(label, ', n(%)')

  # True/yes Subset
  data_subset <-
    if (!inverse) dplyr::filter(data, !! rlang::sym(col))
    else dplyr::filter(data, !(!! rlang::sym(col)))

  # Overall statistic
  overall_stat <- utile.tools::paste_freq(
    count = data_subset,
    total =
      if (remove.na) dplyr::filter(.data = data, !is.na(!! rlang::sym(col)))
    else data,
    percent.sign = percent.sign,
    digits = digits
  )

  # Summarize strata
  if (!is.null(by))
    dplyr::bind_cols(
      Variable = label,
      Overall = overall_stat,

      purrr::map_dfc(
        sort(levels(data[[by]])), # Strata
        function (x) {
          data_by <- dplyr::filter(data, !! rlang::sym(by) == x) # Stratum subset
          tibble::tibble(

            # Stratum statistic
            !!x := utile.tools::paste_freq(
              count = dplyr::filter(data_subset, !! rlang::sym(by) == x),
              total =
                if (remove.na)
                  dplyr::filter(.data = data_by, !is.na(!! rlang::sym(col)))
                else data_by,
              percent.sign = percent.sign,
              digits = digits
            ),

            # Stratum missing count
            !!paste0('Missing: ', x) := utile.tools::paste_freq(
              count = dplyr::filter(.data = data_by, is.na(!! rlang::sym(col))),
              total = data_by,
              percent.sign = percent.sign,
              digits = digits
            )

          )
        }
      ),

      # Strata test
      Probability = utile.tools::test_factor(
        col = col, by = by, parametric = parametric,
        data = data, digits = digits, p.digits = p.digits
      ),

      # Strata test labels
      if (label.stats) tibble::tibble(
        Test = if (!parametric) 'Chisq {NP}' else 'Fisher\'s {P}'
      )

    )

  # Overall summary w/o strata
  else
    dplyr::bind_cols(
      Variable = label,
      Overall = overall_stat,
      Missing = utile.tools::paste_freq(
        count = dplyr::filter(data, is.na(!! rlang::sym(col))),
        total = data,
        percent.sign = percent.sign,
        digits = digits
      )
    )
}

# Create coxph summary row
.row_coxph <- function (label, col, fit, percent.sign, digits, p.digits) {

  # Hard stop
  if (is.null(fit) | class(fit) != 'coxph') stop('Missing valid coxph object. [check: fit]')

  model_table <- utile.tools::tabulate_model(fit = fit, format = FALSE)

  dplyr::mutate_all(
    dplyr::transmute(
      .data = dplyr::filter(model_table, variable == col),
      Variable = purrr::map2_chr(
        variable, level,
        function(x, y)
          if (!is.na(y)) {
            if (nrow(model_table[model_table$variable == x,]) == 1)
              paste(if (!is.null(label)) label else x, y, sep = ', ')
            else paste0('   ', y)
          } else if (!is.null(label)) label
          else x
      ),
      Subjects = subjects,
      Events = utile.tools::paste_freq(
        count = events, total = subjects, percent.sign = percent.sign, digits = digits
      ),
      `HR [95%CI]` = ifelse(
        !is.na(estimate),
        paste(
          round(estimate, digits = digits),
          ifelse(
            !is.na(conf.lower) & !is.na(conf.upper),
            paste0('[', round(conf.lower, digits = digits), '-', round(conf.upper, digits = digits), ']'),
            ''
          )
        ),
        NA
      ),
      p = ifelse(
        !is.na(p),
        format.pval(pv = p, digits = digits, eps = 0.0001, nsmall = p.digits, scientific = F),
        NA
      )
    ),
    as.character
  )

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
