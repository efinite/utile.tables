#' @title Build summary rows
#' @description Summarize a data into a data.frame row(s). Optional
#' stratification and null hypothesis testing using a factor or logical.
#' @param x An object of a supported class. See S3 methods below.
#' @param ... Arguments passed to the appropriate S3 method.
#' @return A \code{\link[tibble:tibble]{tibble::tibble()}} summarizing the
#' provided data.
#' @seealso
#' \code{\link{build_row.data.frame}},
#' \code{\link{build_row.numeric}},
#' \code{\link{build_row.logical}},
#' \code{\link{build_row.factor}}
#' @export
build_row <- function (x, ...) UseMethod('build_row')


#' @export
build_row.default <- function (x, label = NULL, ...) {
  warning('Warning: \'', label, '\' <', class(x), '> not summarizable. Ignoring.')
  NULL
}


#' @title Summarize a data.frame or tibble
#' @description Summarize a data.frame (row counts). Optional stratification
#' using a factor or logical with the same size as the tibble.
#' @param x An data.frame object. Data to summarize. Must be the same length as
#' \code{y} (if specified).
#' @param y A factor or logical. Optional. Data to stratify \code{x} by.
#' @param label A character. Optional. The name of the summarized variable.
#' @param percent.sign A logical. Optional. Paste a percentage symbol with each
#' frequency.
#' @param digits An integer. Optional. Number of digits to round to.
#' @param ... Miscellaneous options.
#' @return A \code{\link[tibble:tibble]{tibble::tibble()}} summarizing the
#' provided data.
#' @examples
#' library(dplyr)
#'
#' data_mtcars <- datasets::mtcars %>%
#'   mutate_at(vars('vs', 'am'), as.logical) %>%
#'   mutate_at(vars('gear', 'carb', 'cyl'), as.factor)
#'
#' # Create a "count" row from a data.frame
#' build_row(x = data_mtcars, y = data_mtcars$cyl)
#' @export
build_row.data.frame <- function (
  x,
  y,
  label = 'n(%)',
  percent.sign = TRUE,
  digits = 1,
  ...
) {

  # Convert logical by group to factor
  if (is.logical(y) & length(y) > 1) y <- .explicit_na(as.factor(y))

  # Build row
  dplyr::bind_cols(

    # Variable label
    Variable = label,

    # Overall count
    Overall = as.character((overall_cnt <- nrow(x))),

    # Frequencies by level
    if (is.factor(y)) {
      purrr::map(
        sort(rlang::set_names(levels(y))),
        ~ utile.tools::paste_freq(
          x = nrow(x[y == .x,]),
          y = overall_cnt,
          percent.sign = percent.sign,
          digits = digits
        )
      )
    }

  )

}


#' @title Summarize numeric data
#' @description Summarize numeric data in a tibble. Optional stratification and
#' null hypothesis testing using another factor or logical.
#' @param x A numeric. Data to summarize. Must be the same length as \code{y}
#' (if specified).
#' @param y A factor or logical. Optional. Data to stratify \code{x} by.
#' @param label A character. Optional. The name of the summarized variable.
#' @param parametric A logical. Optional. Use parametric tests.
#' @param append.stat A logical. Optional. Append the summary statistic used to
#' the label of the summarized row.
#' @param show.missing A logical. Optional. Append summary counts of missing
#' data.
#' @param show.test A logical. Optional. Show the statistical test and test
#' statistic used to determine the p-value.
#' @param percent.sign A logical. Optional. Paste a percentage symbol with each
#' frequency.
#' @param digits An integer. Optional. Number of digits to round to.
#' @param p.digits An integer. Optional. Number of p-value digits to report.
#' @param ... Miscellaneous options.
#' @return A \code{\link[tibble:tibble]{tibble::tibble()}} summarizing the
#' provided data.
#' @seealso \code{\link{build_row}}
#' @examples
#' library(dplyr)
#'
#' data_mtcars <- datasets::mtcars %>%
#'   mutate_at(vars('vs', 'am'), as.logical) %>%
#'   mutate_at(vars('gear', 'carb', 'cyl'), as.factor)
#'
#' # Create a row summarizing a numeric by a factor
#' build_row(x = data_mtcars$mpg, y = data_mtcars$cyl)
#' @export
build_row.numeric <- function(
  x,
  y = NA,
  label = '(Unlabeled column)',
  parametric = FALSE,
  append.stat = TRUE,
  show.missing = FALSE,
  show.test = FALSE,
  percent.sign = TRUE,
  digits = 1,
  p.digits = 4,
  ...
) {

  # Convert logical by group to factor
  if (is.logical(y) & length(y) > 1) y <- .explicit_na(as.factor(y))

  # Statistic factory
  paste_stat_ <- function (...) {
    if (!parametric) utile.tools::paste_median(..., digits = digits)
    else utile.tools::paste_mean(..., digits = digits)
  }
  paste_freq_ <- function (...) {
    utile.tools::paste_freq(
      ...,
      percent.sign = percent.sign,
      digits = digits
    )
  }

  # Build row
  dplyr::bind_cols(

    # Variable label +/- statistic name
    Variable = paste0(
      label,
      if (append.stat & !parametric) ', median[IQR]'
      else if (append.stat & parametric) ', mean\u00B1SD'
    ),

    # Overall summary statistic
    Overall = paste_stat_(x = x),

    # Statistics for by levels
    if (is.factor(y)) {
      purrr::map(
        rlang::set_names(levels(y)),
        ~ paste_stat_(x = x[y == .x])
      )
    },

    if (show.missing) {
      c(

        # Missing overall
        list(
          `Missing: Overall` = paste_freq_(
            x = length(x[is.na(x)]),
            y = length(x)
          )
        ),

        # Missing within by levels
        if (is.factor(y)) {
          purrr::map(
            rlang::set_names(levels(y), paste('Missing:', levels(y))),
            ~ paste_freq_(x = length(x[y == .x & is.na(x)]), y = length(x[y == .x]))
          )
        }

      )
    },

    # Testing with by variable
    if (is.factor(y)) {
      if (length(levels(y)) > 1) {
        c(
          list(
            p = suppressWarnings(
              utile.tools::test_hypothesis(
                x = x, y = y, parametric = parametric, digits = digits,
                p.digits = p.digits
              )
            )
          ),
          if (show.test) {
            list(Test = if (!parametric) 'Wilcox {NP}' else 'Student\'s {P}')
          }
        )
      }
    }

  )

}


#' @title Summarize logical data
#' @description Summarize logical data in a tibble. Optional stratification and
#' null hypothesis testing using another factor or logical.
#' @param x A logical. Data to summarize. Must be the same length as \code{y}
#' (if specified).
#' @param y A factor or logical. Optional. Data to stratify \code{x} by.
#' @param label A character. Optional. The name of the summarized variable.
#' @param inverse A logical. Optional. Report frequencies of the \code{FALSE}
#' values instead.
#' @param parametric A logical. Optional. Use parametric tests.
#' @param na.rm A logical. Optional. Whether to ignore NA values in frequency
#' calculations. If left unspecified, NA values will be given an explicit level
#' and summarized.
#' @param append.stat A logical. Optional. Append the summary statistic used to
#' the label of the summarized row.
#' @param show.missing A logical. Optional. Append summary counts of missing
#' data.
#' @param show.test A logical. Optional. Show the statistical test and test
#' statistic used to determine the p-value.
#' @param percent.sign A logical. Optional. Paste a percentage symbol with each
#' frequency.
#' @param digits An integer. Optional. Number of digits to round to.
#' @param p.digits An integer. Optional. Number of p-value digits to report.
#' @param ... Miscellaneous options.
#' @return A \code{\link[tibble:tibble]{tibble::tibble()}} summarizing the
#' provided data.
#' @seealso \code{\link{build_row}}
#' @examples
#' library(dplyr)
#'
#' data_mtcars <- datasets::mtcars %>%
#'   mutate_at(vars('vs', 'am'), as.logical) %>%
#'   mutate_at(vars('gear', 'carb', 'cyl'), as.factor)
#'
#' # Create a row summarizing a logical by a factor
#' build_row(x = data_mtcars$vs, y = data_mtcars$cyl)
#' @export
build_row.logical <- function (
  x,
  y = NA,
  label = '(Unlabeled column)',
  inverse = FALSE,
  parametric = FALSE,
  na.rm = FALSE,
  append.stat = TRUE,
  show.missing = FALSE,
  show.test = FALSE,
  percent.sign = TRUE,
  digits = 1,
  p.digits = 4,
  ...
) {

  # Set inverse, if applicable
  if (inverse) x <- !x

  # Statistic factory
  paste_stat_ <- function (...) {
    utile.tools::paste_freq(
      ...,
      na.rm,
      percent.sign = percent.sign,
      digits = digits
    )
  }

  # Build row
  dplyr::bind_cols(

    # Variable label +/- statistic name
    Variable = paste0(
      label,
      if (!inverse) { ', yes' } else { ', no' },
      if (append.stat) { ', n(%)' }
    ),

    # Overall summary statistic
    Overall = paste_stat_(x = x[x], y = x),

    # Statistics for by levels
    if (is.factor(y)) {
      purrr::map(
        rlang::set_names(levels(y)),
        ~ paste_stat_(x = x[x & y == .x], y = x[y == .x])
      )
    },

    if (show.missing) {
      c(

        # Missing overall
        list(
          `Missing: Overall` = paste_stat_(
            x = length(x[is.na(x)]),
            y = length(x)
          )
        ),

        # Missing within by levels
        if (is.factor(y)) {
          purrr::map(
            rlang::set_names(levels(y), paste('Missing:', levels(y))),
            ~ paste_stat_(x = length(x[y == .x & is.na(x)]), y = length(x[y == .x]))
          )
        }

      )
    },

    # Testing with by variable
    if (is.factor(y)) {
      if (length(levels(y)) > 1) {
        c(
          list(
            p = suppressWarnings(
              utile.tools::test_hypothesis(
                x = x, y = y, parametric = parametric, digits = digits,
                p.digits = p.digits
              )
            )
          ),
          if (show.test) {
            list(Test = if (!parametric) 'Chisq {NP}' else 'Fisher\'s {P}')
          }
        )
      }
    }

  )

}


#' @title Summarize factor data
#' @description Summarize factor data in a tibble. Optional stratification and
#' null hypothesis testing using another factor or logical.
#' @param x A factor. Data to summarize. Must be the same length as \code{y}
#' (if specified).
#' @param y A factor or logical. Optional. Data to stratify \code{x} by.
#' @param label A character. Optional. The name of the summarized variable.
#' @param parametric A logical. Optional. Use parametric tests.
#' @param na.rm A logical. Optional. Whether to ignore NA values in frequency
#' calculations. If left unspecified, NA values will be given an explicit level
#' and summarized.
#' @param append.stat A logical. Optional. Append the summary statistic used to
#' the label of the summarized row.
#' @param show.missing A logical. Optional. Append summary counts of missing
#' data.
#' @param show.test A logical. Optional. Show the statistical test and test
#' statistic used to determine the p-value.
#' @param percent.sign A logical. Optional. Paste a percentage symbol with each
#' frequency.
#' @param digits An integer. Optional. Number of digits to round to.
#' @param p.digits An integer. Optional. Number of p-value digits to report.
#' @param ... Miscellaneous options.
#' @return A \code{\link[tibble:tibble]{tibble::tibble()}} summarizing the
#' provided data.
#' @seealso \code{\link{build_row}}
#' @examples
#' library(dplyr)
#'
#' data_mtcars <- datasets::mtcars %>%
#'   mutate_at(vars('vs', 'am'), as.logical) %>%
#'   mutate_at(vars('gear', 'carb', 'cyl'), as.factor)
#'
#' # Create a row summarizing a factor by a factor
#' build_row(x = data_mtcars$carb, y = data_mtcars$cyl)
#' @export
build_row.factor <- function (
  x,
  y = NA,
  label = '(Unlabeled column)',
  parametric = FALSE,
  na.rm = FALSE,
  append.stat = TRUE,
  show.missing = FALSE,
  show.test = FALSE,
  percent.sign = TRUE,
  digits = 1,
  p.digits = 4,
  ...
) {

  # Statistic factory
  paste_stat_ <- function (...) {
    utile.tools::paste_freq(
      ...,
      na.rm = na.rm,
      percent.sign = percent.sign,
      digits = digits
    )
  }

  # Level helpers
  x_levels <- levels(x)
  level_fill <- rep('', length(x_levels))

  # Build row
  dplyr::bind_cols(

    # Variable label +/- statistic name
    Variable = c(
      paste0(label, if (append.stat) { ', n(%)' }),
      paste0('  ', x_levels)
    ),

    # Overall summary statistic
    Overall = c(
      '',
      purrr::map_chr(x_levels, ~ paste_stat_(x = x[x == .x], y = x))
    ),

    # Statistics for by levels
    if (is.factor(y)) {
      purrr::map(
        rlang::set_names(levels(y)),
        function (.y) {
          purrr::map_chr(
            c(NA, x_levels),
            function (.x) {
              if (!is.na(.x)) {
                paste_stat_(x = x[x == .x & y == .y], y = x[y == .y])
              } else ''
            }
          )
        }
      )
    },


    if (show.missing) {
      c(

        # Missing overall
        list(
          `Missing: Overall` = c(
            paste_stat_(
              x = length(x[is.na(x)]),
              y = length(x)
            ),
            level_fill
          )
        ),

        # Missing within by levels
        if (is.factor(y)) {
          purrr::map(
            rlang::set_names(levels(y), paste('Missing:', levels(y))),
            ~ c(
              paste_stat_(
                x = length(x[y == .x & is.na(x)]),
                y = length(x[y == .x])
              ),
              level_fill
            )
          )
        }

      )
    },

    # Testing with by variable
    if (is.factor(y)) {
      if (length(levels(y)) > 1) {
        c(
          list(
            p = c(
              suppressWarnings(
                utile.tools::test_hypothesis(
                  x = x, y = y, parametric = parametric, digits = digits,
                  p.digits = p.digits
                )
              ),
              level_fill
            )
          ),
          if (show.test) {
            list(
              Test = c(
                if (!parametric) 'Chisq {NP}' else 'Fisher\'s {P}',
                level_fill
              )
            )
          }
        )
      }
    }

  )

}

