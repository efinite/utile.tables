data_mtcars <- dplyr::mutate_at(
  .tbl = dplyr::mutate_at(
    .tbl = dplyr::as_tibble(datasets::mtcars),
    .vars = dplyr::vars('vs', 'am'),
    .funs = as.logical
  ),
  .vars = dplyr::vars('gear', 'carb', 'cyl'),
  .funs = as.factor
)

testthat::test_that("build_row: numeric", {
  testthat::expect_identical(
    build_row(label = 'Miles per gallon', col = 'mpg', data = data_mtcars),
    tibble::tibble(
      Variable = 'Miles per gallon, median[IQR]',
      Overall = '19.2 [15.4-22.8]',
      Missing = '0 (0%)'
    )
  )
})

testthat::test_that("build_row: numeric, by", {
  testthat::expect_warning(
    testthat::expect_identical(
      build_row(
        label = 'Miles per gallon',
        col = 'mpg',
        by = 'am',
        data = data_mtcars
      )[,-7],
      tibble::tibble(
        Variable = 'Miles per gallon, median[IQR]',
        Overall = '19.2 [15.4-22.8]',
        `FALSE` = '17.3 [14.9-19.2]',
        `TRUE` = '22.8 [21-30.4]',
        `Missing: FALSE` = '0 (0%)',
        `Missing: TRUE` = '0 (0%)',
        Test = 'Wilcox {NP}'
      )
    ),
    'cannot compute exact p-value with ties'
  )
})

testthat::test_that("build_row: factor", {
  testthat::expect_identical(
    build_row(label = 'Cylinders', col = 'cyl', data = data_mtcars),
    tibble::tibble(
      Variable = c('Cylinders, n(%)', '  4', '  6', '  8'),
      Overall = c('', '11 (34.4%)', '7 (21.9%)', '14 (43.8%)'),
      Missing = c('0 (0%)', '', '', '')
    )
  )
})

testthat::test_that("build_row: factor, by", {
  testthat::expect_identical(
    build_row(
      label = 'Cylinders',
      col = 'cyl',
      by = 'am',
      data = data_mtcars
    )[,-7],
    tibble::tibble(
      Variable = c('Cylinders, n(%)', '  4', '  6', '  8'),
      Overall = c('', '11 (34.4%)', '7 (21.9%)', '14 (43.8%)'),
      `FALSE` = c('', '3 (15.8%)', '4 (21.1%)', '12 (63.2%)'),
      `TRUE` = c('', '8 (61.5%)', '3 (23.1%)', '2 (15.4%)'),
      `Missing: FALSE` = c('0 (0%)', '', '', ''),
      `Missing: TRUE` = c('0 (0%)', '', '', ''),
      Test = c('Chisq {NP}', '', '', '')
    )
  )
})

testthat::test_that("build_row: logical", {
  testthat::expect_identical(
    build_row(label = 'Straight Engine', col = 'vs', data = data_mtcars),
    tibble::tibble(
      Variable = 'Straight Engine, yes, n(%)',
      Overall = '14 (43.8%)',
      Missing = '0 (0%)'
    )
  )
})

testthat::test_that("build_row: logical, by", {
  testthat::expect_identical(
    build_row(
      label = 'Straight Engine',
      col = 'vs',
      by = 'am',
      data = data_mtcars
    )[,-7],
    tibble::tibble(
      Variable = 'Straight Engine, yes, n(%)',
      Overall = '14 (43.8%)',
      `FALSE` = '7 (36.8%)',
      `TRUE` = '7 (53.8%)',
      `Missing: FALSE` = '0 (0%)',
      `Missing: TRUE` = '0 (0%)',
      Test = 'Chisq {NP}'
    )
  )
})
