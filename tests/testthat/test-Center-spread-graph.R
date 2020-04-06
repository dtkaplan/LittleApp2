test_that("multiplication works", {
  mtcars["cyls"] <- as.numeric(mtcars[["cyl"]] == 8)
  mtcars["big"] <- as.factor(mtcars$wt > median(mtcars$wt))
  P <- center_spread_graph(cyls ~ big, data = mtcars,
                           labels = c("Small", "EIGHT!"),
                           annots = list(show_ci = TRUE,
                                         show_mean = TRUE,
                                         show_median = TRUE,
                                         show_sd = TRUE,
                                         show_violin = FALSE,
                                         prob_level = 0.90,
                                         show_summary_interval = TRUE
                                         ))
  expect_type(P, type = "ggplot")
  P2 <- center_spread_graph(cyls ~ 1, data = mtcars,
                            labels = c("Small", "EIGHT!"),
                            annots = list(show_ci = TRUE,
                                          show_mean = TRUE,
                                          show_median = TRUE,
                                          show_sd = TRUE,
                                          show_violin = FALSE,
                                          prob_level = 0.90,
                                          show_summary_interval = TRUE
                            ))
  expect_type(P$P, type = "ggplot")
})
