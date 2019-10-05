data_mtcars <- datasets::mtcars %>%
  dplyr::as_tibble() %>%
  dplyr::mutate_at(dplyr::vars('vs', 'am'), as.logical) %>%
  dplyr::mutate_at(dplyr::vars('gear', 'carb', 'cyl'), as.factor)

test_that("build_row: numeric", {
  expect_identical(
    build_row(label = 'Miles per gallon', col = 'mpg', data = data_mtcars),
    tibble::tibble(
      Variable = 'Miles per gallon, median[IQR]',
      Overall = '19.2 [15.4-22.8]',
      Missing = '0 (0%)'
    )
  )
})

test_that("build_row: numeric, by", {
  expect_warning(
    expect_identical(
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

test_that("build_row: factor", {
  expect_identical(
    build_row(label = 'Cylinders', col = 'cyl', data = data_mtcars),
    tibble::tibble(
      Variable = c('Cylinders, n(%)', '  4', '  6', '  8'),
      Overall = c('', '11 (34.4%)', '7 (21.9%)', '14 (43.8%)'),
      Missing = c('0 (0%)', '', '', '')
    )
  )
})

test_that("build_row: factor, by", {
  expect_identical(
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

test_that("build_row: logical", {
  expect_identical(
    build_row(label = 'Straight Engine', col = 'vs', data = data_mtcars),
    tibble::tibble(
      Variable = 'Straight Engine, yes, n(%)',
      Overall = '14 (43.8%)',
      Missing = '0 (0%)'
    )
  )
})

test_that("build_row: logical, by", {
  expect_identical(
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
