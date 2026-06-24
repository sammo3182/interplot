# Feature A: three-way interactions.

test_that("continuous var3 yields three Aiken-West groups", {
  m <- lm(mpg ~ wt * hp * cyl, data = mtcars)
  set.seed(1)
  d <- interplot(m, "wt", "hp", var3 = "cyl", plot = FALSE)$df_coef
  expect_true("value" %in% names(d))
  expect_equal(length(unique(d$value)), 3)
  expect_match(paste(levels(d$value), collapse = " "), "Low|Mean|High")
})

test_that("three-way slice matches the analytic conditional coefficient", {
  m <- lm(mpg ~ wt * hp * cyl, data = mtcars)
  cf <- coef(m)
  c3 <- 6
  set.seed(2)
  d <- interplot(m, "wt", "hp", var3 = "cyl", var3_vals = c3, plot = FALSE)$df_coef
  v2 <- d$wt # column 1 holds the var2 (hp) sequence
  expected <- cf["wt"] + cf["wt:hp"] * v2 + cf["wt:cyl"] * c3 + cf["wt:hp:cyl"] * v2 * c3
  expect_lt(max(abs(d$coef - unname(expected))), 0.3)
})

test_that("factor var3 produces one group per level (incl. reference)", {
  mt <- mtcars
  mt$cyl_f <- factor(mt$cyl)
  m <- lm(mpg ~ wt * hp * cyl_f, data = mt)
  set.seed(3)
  d <- interplot(m, "wt", "hp", var3 = "cyl_f", plot = FALSE)$df_coef
  expect_equal(length(unique(d$value)), 3)
})

test_that("facet and overlay both return ggplot objects", {
  m <- lm(mpg ~ wt * hp * cyl, data = mtcars)
  set.seed(4)
  expect_s3_class(interplot(m, "wt", "hp", var3 = "cyl"), "ggplot")
  expect_s3_class(interplot(m, "wt", "hp", var3 = "cyl", facet = FALSE), "ggplot")
})

test_that("custom var3_vals control the number of groups", {
  m <- lm(mpg ~ wt * hp * cyl, data = mtcars)
  set.seed(5)
  d <- interplot(m, "wt", "hp", var3 = "cyl", var3_vals = c(4, 8), plot = FALSE)$df_coef
  expect_equal(length(unique(d$value)), 2)
})

test_that("three distinct variables are required", {
  m <- lm(mpg ~ wt * hp * cyl, data = mtcars)
  expect_error(interplot(m, "wt", "hp", var3 = "wt"), "distinct")
})
