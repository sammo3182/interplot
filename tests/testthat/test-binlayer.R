# Feature B2: bin_layer() binning diagnostic.

test_that("bin_layer returns a list of ggplot layers", {
  m <- lm(mpg ~ wt * cyl, data = mtcars)
  L <- bin_layer(m, "cyl", "wt")
  expect_type(L, "list")
  expect_length(L, 2)
  expect_s3_class(L[[1]], "ggproto")
  expect_s3_class(L[[2]], "ggproto")
})

test_that("bin_layer composes with an interplot", {
  m <- lm(mpg ~ wt * cyl, data = mtcars)
  set.seed(1)
  p <- interplot(m, "cyl", "wt") + bin_layer(m, "cyl", "wt")
  expect_s3_class(p, "ggplot")
})

test_that("bin_layer respects the number of bins", {
  m <- lm(mpg ~ wt * cyl, data = mtcars)
  expect_length(bin_layer(m, "cyl", "wt", bins = 4), 2)
})

test_that("bin_layer rejects factors and quadratic self-terms", {
  mt <- mtcars
  mt$cyl_f <- factor(mt$cyl)
  m_f <- lm(mpg ~ wt * cyl_f, data = mt)
  expect_error(bin_layer(m_f, "wt", "cyl_f"), "continuous")
  m <- lm(mpg ~ wt * cyl, data = mtcars)
  expect_error(bin_layer(m, "wt", "wt"), "quadratic")
})
