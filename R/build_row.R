#' @title Build summary rows
#' @description Summarize a data into a data.frame row(s). Optional
#' stratification and null hypothesis testing using a factor or logical.
#' @param x An object of a supported class. See S3 methods below.
#' @param ... Arguments passed to the appropriate S3 method.
#' @return An object of class \code{tbl_df} (tibble) summarizing the provided
#' data.
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
#' @param show.missing A logical. Optional. Append an empty missing data column.
#' @param show.test A logical. Optional. Append empty test and statistic columns.
#' @param percent.sign A logical. Optional. Paste a percentage symbol with each
#' frequency.
#' @param digits An integer. Optional. Number of digits to round to.
#' @param ... Miscellaneous options.
#' @return An object of class \code{tbl_df} (tibble) summarizing the provided
#' data.
#' @examples
#' # Create a "count" row from a data.frame for a factor
#' build_row(x = datasets::mtcars, y = as.factor(datasets::mtcars$cyl))
#' @export
build_row.data.frame <- function (
  x,
  y,
  label = 'n(%)',
  show.missing = FALSE,
  show.test = FALSE,
  percent.sign = TRUE,
  digits = 1,
  ...
) {

  # Convert logical by group to factor
  if (is.logical(y) & length(y) > 1) y <- .explicit_na(as.factor(y))


  # Build row
  cols <- list()

  # Variable label
  cols$Variable <- label

  # Overall count
  cols$Overall <- as.character((overall_cnt <- nrow(x)))

  # Frequencies by level
  if (is.factor(y)) {
    cols <- c(
      cols,
      purrr::map(
        sort(rlang::set_names(levels(y))),
        function (.y) {
          utile.tools::paste_freq(
            x = nrow(x[y == .y,]),
            y = overall_cnt,
            percent.sign = percent.sign,
            digits = digits
          )
        }
      )
    )
  }

  # Missing count columns
  if (show.missing) {
    cols$`Missing: Overall` <- ''
    if (is.factor(y)) {
      cols <- c(
        cols,
        rlang::set_names(
          rep('', length(levels(y))),
          paste('Missing:', levels(y))
        )
      )
    }
  }

  # Hypothesis testing columns
  if (is.factor(y)) {
    if (length(levels(y)) > 1) {
      cols$p <- ''
      if (show.test) cols$Test <- ''
    }
  }

  # Return converted tibble
  dplyr::as_tibble(cols)

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
#' @return An object of class \code{tbl_df} (tibble) summarizing the provided
#' data.
#' @seealso \code{\link{build_row}}
#' @examples
#' # Create a row summarizing a numeric by a factor
#' build_row(
#'  x = datasets::mtcars$mpg,
#'  y = as.factor(datasets::mtcars$cyl),
#'  label = 'MPG'
#' )
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

  # Statistic functions
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


  # Convert logical by group to factor
  if (is.logical(y) & length(y) > 1) y <- .explicit_na(as.factor(y))


  # Create column object
  cols <- list()

  # Variable label +/- statistic name
  cols$Variable <- paste0(
    label,
    if (append.stat & !parametric) ', median[IQR]'
    else if (append.stat & parametric) ', mean\u00B1SD'
  )

  # Overall summary statistic
  cols$Overall = paste_stat_(x = x)

  # Statistics for by levels
  if (is.factor(y)) {
    cols <- c(
      cols,
      purrr::map_chr(
        rlang::set_names(levels(y)),
        function (.y) paste_stat_(x = x[y == .y])
      )
    )
  }

  # Missing
  if (show.missing) {

    # Overall
    cols$`Missing: Overall` = paste_freq_(
      x = length(x[is.na(x)]),
      y = length(x)
    )

    # Within strata
    if (is.factor(y)) {
      cols <- c(
        cols,
        purrr::map(
          rlang::set_names(levels(y), paste('Missing:', levels(y))),
          function (.y) {
            paste_freq_(
              x = length(x[y == .y & is.na(x)]),
              y = length(x[y == .y])
            )
          }
        )
      )
    }

  }

  # Hypothesis testing
  if (is.factor(y)) {
    if (length(levels(y)) > 1) {

      # P-values
      cols$p <- suppressWarnings(
        utile.tools::test_hypothesis(
          x = x, y = y, parametric = parametric, digits = digits,
          p.digits = p.digits
        )
      )

      # Test used
      if (show.test) {
        cols$Test <- if (!parametric) 'Wilcox' else 'Student\'s'
      }

    }
  }


  # Return converted tibble
  dplyr::as_tibble(cols)

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
#' @return An object of class \code{tbl_df} (tibble) summarizing the provided
#' data.
#' @seealso \code{\link{build_row}}
#' @examples
#' # Create a row summarizing a logical by a factor
#' build_row(
#'   x = as.logical(datasets::mtcars$vs),
#'   y = as.factor(datasets::mtcars$cyl),
#'   label = 'VS'
#' )
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

  # Statistic factory
  paste_stat_ <- function (...) {
    utile.tools::paste_freq(
      ...,
      na.rm,
      percent.sign = percent.sign,
      digits = digits
    )
  }


  # Set inverse, if applicable
  if (inverse) x <- !x


  # Create column object
  cols <- list()

  # Variable label +/- statistic name
  cols$Variable <- paste0(
    label,
    if (!inverse) { ', yes' } else { ', no' },
    if (append.stat) { ', n(%)' }
  )

  # Overall statistic
  cols$Overall <- paste_stat_(x = x[x], y = x)

  # Strata statistics
  if (is.factor(y)) {
    cols <- c(
      cols,
      purrr::map(
        rlang::set_names(levels(y)),
        function (.y) paste_stat_(x = x[x & y == .y], y = x[y == .y])
      )
    )
  }

  # Missing
  if (show.missing) {

    # Overall
    cols$`Missing: Overall` <- paste_stat_(
      x = length(x[is.na(x)]),
      y = length(x)
    )

    # Within strata
    if (is.factor(y)) {
      cols <- c(
        cols,
        purrr::map(
          rlang::set_names(levels(y), paste('Missing:', levels(y))),
          function (.y) {
            paste_stat_(
              x = length(x[y == .y & is.na(x)]),
              y = length(x[y == .y])
            )
          }
        )
      )
    }

  }

  # Hypothesis testing
  if (is.factor(y)) {
    if (length(levels(y)) > 1) {

      # p-value
      cols$p <- suppressWarnings(
        utile.tools::test_hypothesis(
          x = x, y = y, parametric = parametric, digits = digits,
          p.digits = p.digits
        )
      )

      # statistical tests
      if (show.test) {
        cols$Test <- if (!parametric) 'Chisq' else 'Fisher\'s'
      }

    }
  }


  # Return converted tibble
  dplyr::as_tibble(cols)

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
#' @return An object of class \code{tbl_df} (tibble) summarizing the provided
#' data.
#' @seealso \code{\link{build_row}}
#' @examples
#' # Create a row summarizing a factor by a factor
#' build_row(
#'  x = as.factor(mtcars$carb),
#'  y = as.factor(mtcars$cyl),
#'  label = 'Carb'
#' )
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

  # Statistic function
  paste_stat_ <- function (...) {
    utile.tools::paste_freq(
      ...,
      na.rm = na.rm,
      percent.sign = percent.sign,
      digits = digits
    )
  }


  # Level helpers
  level_fill <- rep('', length((x_levels <- levels(x))))


  # Create column object
  cols <- list()

  # Variable labels
  cols$Variable <- c(
    paste0(label, if (append.stat) { ', n(%)' }),
    paste0('  ', x_levels)
  )

  # Overall summary statistic
  cols$Overall <- c(
    '',
    purrr::map_chr(x_levels, function (.x) paste_stat_(x = x[x == .x], y = x))
  )

  if (is.factor(y)) {
    cols <- c(
      cols,
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
    )
  }

  if (show.missing) {

    # Missing overall
    cols$`Missing: Overall` <- c(
      paste_stat_(
        x = length(x[is.na(x)]),
        y = length(x)
      ),
      level_fill
    )

    # Missing within by levels
    if (is.factor(y)) {
      cols <- c(
        cols,
        purrr::map(
          rlang::set_names(levels(y), paste('Missing:', levels(y))),
          function (.y) {
            c(
              paste_stat_(
                x = length(x[y == .y & is.na(x)]),
                y = length(x[y == .y])
              ),
              level_fill
            )
          }
        )
      )
    }
  }

  # Testing with by variable
  if (is.factor(y)) {
    if (length(levels(y)) > 1) {

      cols$p <- c(
        suppressWarnings(
          utile.tools::test_hypothesis(
            x = x, y = y, parametric = parametric, digits = digits,
            p.digits = p.digits
          )
        ),
        level_fill
      )

      if (show.test) {
        cols$Test <- c(if (!parametric) 'Chisq' else 'Fisher\'s Exact', level_fill)
      }

    }
  }


  # Return converted tibble
  dplyr::as_tibble(cols)

}

