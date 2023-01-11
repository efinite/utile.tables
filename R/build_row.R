#' @title Build summary rows
#' @description Summarize data into a data.frame with row(s). Optional
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
  warning('Warning: \'', label, '\' <', class(x), '> not summarizable. Skipping.')
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
#' @param percent.sign A logical. Optional. Paste a percentage symbol with each
#' frequency.
#' @param digits An integer. Optional. Number of digits to round to.
#' @param ... Miscellaneous options.
#' @param col.overall A logical. Append a column with the statistic for all data.
#' If \code{y} is not specified, this parameter is ignored.
#' @param col.test A logical. Append a column with the name of the statistical
#' @return An object of class \code{tbl_df} (tibble) summarizing the provided
#' data.
#' @examples
#' # Create a "count" row from a data.frame for a factor
#' build_row(x = datasets::mtcars, y = as.factor(datasets::mtcars$cyl))
#' @export
build_row.data.frame <- function (
  x,
  y = NA,
  label = 'n(%)',
  show.missing = FALSE,
  percent.sign = FALSE,
  col.overall = TRUE,
  col.test = FALSE,
  digits = 1,
  ...
) {

  # Retrieve by variable levels
  y_levels <- .get_levels(y)

  # Count rows
  x_cnt <- nrow(x)

  # Build row
  cols <- list()

  # Variable label
  cols$Variable <- label

  # Overall count
  if (col.overall || length(y_levels) == 0) {
    cols$Overall <- as.character(x_cnt)
  }

  # Frequencies by level
  if (length(y_levels) > 0) {
    cols <- c(
      cols,
      purrr::map(
        y_levels,
        function (.y) {
          utile.tools::paste_freq(
            x = nrow(x[y == .y & !is.na(y),]),
            na.rm = FALSE,
            percent.sign = percent.sign,
            digits = digits
            y = x_cnt
          )
        }
      )
    )
  }

  # Missing
  if (show.missing) {
    cols$Missing <- utile.tools::paste_freq(
      x = y[is.na(y)],
      y = y,
      na.rm = FALSE,
      percent.sign = percent.sign,
      digits = digits
    )
  }

  # Hypothesis testing columns
  if (length(y_levels) > 1) cols[c('p', if (col.test) 'Test')] <- ''

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
#' @param append.stat A logical. Optional. Append the summary statistic used to
#' the label of the summarized row.
#' @param show.missing A logical. Optional. Append summary counts of missing
#' data.
#' @param percent.sign A logical. Optional. Paste a percentage symbol with each
#' frequency.
#' @param digits An integer. Optional. Number of digits to round to.
#' @param p.digits An integer. Optional. Number of p-value digits to report.
#' @param ... Miscellaneous options.
#' @param stat A character. Name of the summary statistic to use. Supported options
#' include the mean ('mean') and median ('median').
#' @param col.overall A logical. Append a column with the statistic for all data.
#' If \code{y} is not specified, this parameter is ignored.
#' @param test A character. Name of statistical test to compare groups.
#' Supported options include ANOVA linear model ('anova'), Kruskal-Wallis ('kruskal'),
#' and Wilcoxon rank sum ('wilcoxon') tests.
#' @param col.test A logical. Append a column with the name of the statistical
#' test used.
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
build_row.numeric <- function (
  x,
  y = NA,
  label = '(Unlabeled column)',
  append.stat = TRUE,
  show.missing = FALSE,
  percent.sign = FALSE,
  stat = c('mean', 'median'),
  col.overall = TRUE,
  test = c('anova', 'kruskal', 'wilcoxon'),
  col.test = FALSE,
  digits = 1,
  p.digits = 4,
  ...
) {

  # Check arguments
  stat <- match.arg(stat)
  test <- match.arg(test)

  # Statistic functions
  paste_stat_ <- function (...) {
    if (stat == 'median') utile.tools::paste_median(..., digits = digits)
    else utile.tools::paste_mean(..., digits = digits)
  }

  paste_freq_ <- function (...) {
    utile.tools::paste_freq(
      ...,
      na.rm = FALSE,
      percent.sign = percent.sign,
      digits = digits
    )
  }

  # Retrieve by variable levels
  y_levels <- .get_levels(y)

  # Create column object
  cols <- list()

  # Variable label +/- statistic name
  cols$Variable <- paste0(
    label,
      if (stat == 'median') ', median[IQR]'
      else ', mean\u00B1SD'
  )

  # Summary statistic: Overall
  if (col.overall | length(y_levels) == 0) cols$Overall <- paste_stat_(x = x)

  # Statistics for by levels
  if (length(y_levels) > 0) {
    cols <- c(
      cols,
      purrr::map_chr(
        y_levels,
        function (.y) paste_stat_(x = x[!is.na(x) & y %in% .y])
      )
    )
  }

  # Missing
  if (show.missing) {
    cols$Missing <- paste_freq_(x = sum(is.na(x)), y = length(x))
  }

  # Hypothesis testing
  if (length(y_levels) > 1) {

    # Statistical test
    test <- utile.tools::test_hypothesis(
      x = x,
      y = y,
      test = test,
      digits = digits,
      p.digits = p.digits
    )

    # Addend test data
    cols$p <- test$p
    if (col.test) cols$Test <- test$test

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
#' @param append.stat A logical. Optional. Append the summary statistic used to
#' the label of the summarized row.
#' @param show.missing A logical. Optional. Append summary counts of missing
#' data.
#' @param percent.sign A logical. Optional. Paste a percentage symbol with each
#' frequency.
#' @param digits An integer. Optional. Number of digits to round to.
#' @param p.digits An integer. Optional. Number of p-value digits to report.
#' @param ... Miscellaneous options.
#' @param col.overall A logical. Append a column with the statistic for all data.
#' If \code{y} is not specified, this parameter is ignored.
#' @param test A character. Name of statistical test to compare groups.
#' Supported options include Pearson's Chi-squared Test ('chisq') and Fisher's
#' Exact Test ('fisher').
#' @param test.simulate.p A logical. Whether to use Monte Carlo simulation of
#' the p-value when testing nominal data.
#' @param col.test A logical. Append a column with the name of the statistical
#' test used.
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
  append.stat = TRUE,
  show.missing = FALSE,
  percent.sign = FALSE,
  col.overall = TRUE,
  test = c('chisq', 'fisher'),
  test.simulate.p = FALSE,
  col.test = FALSE,
  digits = 1,
  p.digits = 4,
  ...
) {

  # Check arguments
  test <- match.arg(test)

  # Statistic factory
  paste_stat_ <- function (...) {
    utile.tools::paste_freq(
      ...,
      na.rm = FALSE,
      percent.sign = percent.sign,
      digits = digits
    )
  }

  # Retrieve by variable levels
  y_levels <- .get_levels(y)

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
  if (col.overall || length(y_levels) == 0) {
    cols$Overall <- paste_stat_(x = x[x & !is.na(x)], y = x)
  }

  # Strata statistics
  if (length(y_levels) > 0) {
    cols <- c(
      cols,
      purrr::map(
        y_levels,
        function (.y) {
          paste_stat_(
            x = x[(x & !is.na(x)) & (y %in% .y)],
            y = x[y %in% .y]
          )
        }
      )
    )
  }

  # Missing
  if (show.missing) {
    cols$Missing <- paste_stat_(x = x[is.na(x)], y = x)
  }

  # Hypothesis testing
  if (length(y_levels) > 1) {

    # Statistical test
    test <- utile.tools::test_hypothesis(
      x = x,
      y = y,
      test = test,
      simulate.p = test.simulate.p,
      digits = digits,
      p.digits = p.digits
    )

    # Addend test data
    cols$p <- test$p
    if (col.test) cols$Test <- test$test

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
#' @param append.stat A logical. Optional. Append the summary statistic used to
#' the label of the summarized row.
#' @param show.missing A logical. Optional. Append summary counts of missing
#' data.
#' @param percent.sign A logical. Optional. Paste a percentage symbol with each
#' frequency.
#' @param digits An integer. Optional. Number of digits to round to.
#' @param p.digits An integer. Optional. Number of p-value digits to report.
#' @param ... Miscellaneous options.
#' @param col.overall A logical. Append a column with the statistic for all data.
#' If \code{y} is not specified, this parameter is ignored.
#' @param test A character. Name of statistical test to compare groups.
#' Supported options include Pearson's Chi-squared Test ('chisq') and Fisher's
#' Exact Test ('fisher').
#' @param test.simulate.p A logical. Whether to use Monte Carlo simulation of
#' the p-value when testing nominal data.
#' @param col.test A logical. Append a column with the name of the statistical
#' test used.
#' @return An object of class \code{tbl_df} (tibble) summarizing the provided
#' data.
#' @seealso \code{\link{build_row}}
#' @examples
#' # Create a row summarizing a factor by a factor
#' build_row(
#'  x = as.factor(datasets::mtcars$carb),
#'  y = as.factor(datasets::mtcars$cyl),
#'  label = 'Carb'
#' )
#' @export
build_row.factor <- function (
  x,
  y = NA,
  label = '(Unlabeled column)',
  append.stat = TRUE,
  show.missing = FALSE,
  percent.sign = FALSE,
  col.overall = TRUE,
  test = c('chisq', 'fisher'),
  test.simulate.p = FALSE,
  col.test = FALSE,
  digits = 1,
  p.digits = 4,
  ...
) {

  # Check arguments
  test <- match.arg(test)

  # Statistic function
  paste_stat_ <- function (...) {
    utile.tools::paste_freq(
      ...,
      na.rm = FALSE,
      percent.sign = percent.sign,
      digits = digits
    )
  }

  # Retrieve y variable levels
  y_levels <- .get_levels(y)

  # Identify x levels, make any NA explicit
  x_levels <- .get_levels(x)
  if (any(is.na(x))) x_levels <- c(x_levels, "Missing" = NA)
  level_fill <- rep('', length(x_levels))

  # Create column object
  cols <- list()

  # Variable labels
  cols$Variable <- c(
    paste0(label, if (append.stat) { ', n(%)' }),
    paste0('  ', names(x_levels))
  )

  # Overall summary statistic
  if (col.overall || length(y_levels) == 0) {
    cols$Overall <- c(
      '',
      purrr::map_chr(x_levels, function (.x) {
        paste_stat_(x = x[x %in% .x], y = x)
      })
    )
  }

  if (length(y_levels) > 0) {
    cols <- c(
      cols,
      purrr::map(
        y_levels,
        function (.y) {
          c(
            '',
            purrr::map_chr(
              x_levels,
              function (.x) {
                paste_stat_(
                  x = x[x %in% .x & y %in% .y],
                  y = x[y %in% .y]
                )
              }
            )
          )
        }
      )
    )
  }

  # Show missing count
  if (show.missing) {
    cols$Missing <- c(paste_stat_(x = x[is.na(x)], y = x), level_fill)
  }

  # Testing with by variable
  if (length(y_levels) > 1) {

    # Statistical test
    test <- utile.tools::test_hypothesis(
      x = x,
      y = y,
      test = test,
      simulate.p = test.simulate.p,
      digits = digits,
      p.digits = p.digits
    )

    # Addend test data
    cols$p <- c(test$p, level_fill)
    if (col.test) cols$Test <- c(test$test, level_fill)

  }

  # Return converted tibble
  dplyr::as_tibble(cols)

}

