# Three-way + nonlinear extended to mixed-effects and multiply-imputed methods.
# (brms is excluded here: it requires Stan compilation, too slow for tests.)

test_that("lmer three-way slice matches the fixef formula", {
  skip_if_not_installed("lme4")
  m <- suppressMessages(lme4::lmer(mpg ~ wt * hp * cyl + (1 | carb), data = mtcars))
  set.seed(1)
  c3 <- 6
  d <- interplot(m, "wt", "hp", var3 = "cyl", var3_vals = c3, plot = FALSE)$df_coef
  fe <- lme4::fixef(m)
  v2 <- d$wt
  expected <- fe["wt"] + fe["wt:hp"] * v2 + fe["wt:cyl"] * c3 + fe["wt:hp:cyl"] * v2 * c3
  expect_lt(max(abs(d$coef - unname(expected))), 0.5)
  expect_equal(length(unique(d$value)), 1L)
})

test_that("lmer nonlinear (polynomial) matches the fixef gradient", {
  skip_if_not_installed("lme4")
  m <- suppressMessages(lme4::lmer(mpg ~ wt * (hp + I(hp^2)) + (1 | carb), data = mtcars))
  set.seed(2)
  d <- interplot(m, "wt", "hp", plot = FALSE)$df_coef
  fe <- lme4::fixef(m)
  z <- d$wt
  expected <- fe["wt"] + fe["wt:hp"] * z + fe["wt:I(hp^2)"] * z^2
  expect_lt(max(abs(d$coef - unname(expected))), 0.5)
})

test_that("lmer nonlinear spline returns a ggplot", {
  skip_if_not_installed("lme4")
  m <- suppressMessages(lme4::lmer(mpg ~ wt * splines::ns(hp, 3) + (1 | carb), data = mtcars))
  set.seed(3)
  expect_s3_class(interplot(m, "wt", "hp"), "ggplot")
})

test_that("lmmi three-way pools across imputations", {
  skip_if_not_installed("lme4")
  set.seed(7)
  imps <- lapply(1:3, function(i) {
    d <- mtcars; d$mpg <- d$mpg + rnorm(nrow(d), 0, 0.5); d
  })
  mi <- lapply(imps, function(d) lm(mpg ~ wt * hp * cyl, data = d))

  d <- interplot(mi, "wt", "hp", var3 = "cyl", var3_vals = 6, plot = FALSE)$df_coef
  me_each <- vapply(mi, function(m) {
    cf <- coef(m); unname(cf["wt"] + cf["wt:hp"] * d$wt + cf["wt:cyl"] * 6 + cf["wt:hp:cyl"] * d$wt * 6)
  }, numeric(nrow(d)))
  expect_lt(max(abs(d$coef - rowMeans(me_each))), 0.5)
})

test_that("lmmi nonlinear pools across imputations", {
  set.seed(8)
  imps <- lapply(1:3, function(i) {
    d <- mtcars; d$mpg <- d$mpg + rnorm(nrow(d), 0, 0.5); d
  })
  mi <- lapply(imps, function(d) lm(mpg ~ wt * (hp + I(hp^2)), data = d))

  d <- interplot(mi, "wt", "hp", plot = FALSE)$df_coef
  me_each <- vapply(mi, function(m) {
    cf <- coef(m); unname(cf["wt"] + cf["wt:hp"] * d$wt + cf["wt:I(hp^2)"] * d$wt^2)
  }, numeric(nrow(d)))
  expect_lt(max(abs(d$coef - rowMeans(me_each))), 0.5)
})
