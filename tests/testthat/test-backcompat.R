# Regression guard: the existing two-way linear path must keep working exactly
# as before once var3 / nonlinear branches are added. Uses analytic checks
# (robust to R/RNG version) plus within-run determinism.

test_that("two-way linear interplot is reproducible (fixed seed)", {
  m <- lm(mpg ~ wt * cyl, data = mtcars)
  set.seed(324)
  d1 <- interplot(m, "cyl", "wt", plot = FALSE)$df_coef
  set.seed(324)
  d2 <- interplot(m, "cyl", "wt", plot = FALSE)$df_coef
  expect_equal(d1, d2)
})

test_that("two-way linear point estimates match analytic b1 + b12 * z", {
  m <- lm(mpg ~ wt * cyl, data = mtcars)
  set.seed(324)
  d <- interplot(m, "cyl", "wt", plot = FALSE)$df_coef

  expect_named(d, c("cyl", "coef", "ub", "lb"))
  expect_true(all(d$ub >= d$coef & d$coef >= d$lb))

  # Column 1 ("cyl") holds the var2 (wt) sequence; the conditional coefficient
  # of cyl is b_cyl + b_{wt:cyl} * wt.
  expected <- coef(m)["cyl"] + coef(m)["wt:cyl"] * d$cyl
  expect_equal(d$coef, unname(expected), tolerance = 0.15)
})

test_that("var3 = NULL is byte-for-byte identical to omitting var3", {
  m <- lm(mpg ~ wt * cyl, data = mtcars)
  set.seed(99)
  d_omit <- interplot(m, "cyl", "wt", plot = FALSE)$df_coef
  set.seed(99)
  d_null <- interplot(m, "cyl", "wt", var3 = NULL, plot = FALSE)$df_coef
  expect_equal(d_omit, d_null)
})
