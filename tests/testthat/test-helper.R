


test_that("formula construction works", {
  expect_equal(
    get_model_formula(mtcars[,1:3], interaction=FALSE, model_order=1),
    mpg ~ cyl + disp)
  expect_equal(
    get_model_formula(mtcars[,1:3], interaction=TRUE, model_order=1),
    mpg ~ cyl * disp)
  expect_equal(
    get_model_formula(mtcars[,1:3], interaction=TRUE, model_order=2),
    mpg ~ ns(cyl, 2) * disp)
  expect_equal(
    get_model_formula(mtcars[,1:3], interaction=TRUE, model_order=2,
                      model_type = "rpart"),
    mpg ~ ns(cyl, 2) + disp)
})

