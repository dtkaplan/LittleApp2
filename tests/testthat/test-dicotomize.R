test_that("dichotomization works", {
  Many_types <-
    data.frame(
      open = c(142.25, 141.23, 141.33, 140.82, 141.31,
               140.58),
      close = c(141.37, 141.67, 140.54, 141.19, 141.07, 141.540),
      volume = c(94807, 69620, 76645, 71655, 75680, 72428),
      date = c("2007-01-03", "2007-01-04", "2007-01-05", "2007-01-08", "2007-01-09", "2007-01-10"),
      stringsAsFactors = FALSE
    ) %>%
    dplyr::mutate(direction = ifelse(open > close, "up", "down"),
           volume = round(volume, -4),
           sales = paste0('$', format(volume, big.mark = ",")),
           date = as.Date(date)
    )

  expect_equal(length(levels(dichotomize(Many_types$open))), 2)
  expect_true(
    is.numeric(dichotomize(Many_types$open, force = FALSE)))
  expect_false(is.numeric(dichotomize(Many_types$open)))
  expect_equal(length(levels(dichotomize(Many_types$date))), 2)
  expect_true(inherits(dichotomize(Many_types$date, force = FALSE), "Date"))
  expect_equal(levels(dichotomize(Many_types$sales))[1], "$70,000")
  for_logical <- dichotomize(Many_types$open  > median(Many_types$open,  na.rm=TRUE))
  expect_equal(attr(for_logical, "levels"), c("false",  "true"))
})
