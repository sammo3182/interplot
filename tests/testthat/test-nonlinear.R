# Feature B1: nonlinear-in-moderator conditional effects (gradient engine).

test_that("the gradient engine reproduces the linear engine exactly", {
  set.seed(1)
  sv1 <- rnorm(500, 2, 0.5)
  sv12 <- rnorm(500, -0.3, 0.1)
  fake <- seq(1, 5, length.out = 30)

  a <- sim_coef_vec(sv1, sv12, fake, 0.95)
  b <- sim_coef_grad(cbind(sv1, sv12), cbind(1, fake), fake, 0.95)
  expect_equal(a$coef1, b$coef1, tolerance = 1e-12)
  expect_equal(a$ub, b$ub, tolerance = 1e-12)
  expect_equal(a$lb, b$lb, tolerance = 1e-12)
})

test_that("the gradient engine reproduces the quadratic (multiplier = 2) engine", {
  set.seed(2)
  sv1 <- rnorm(500, 1, 0.4)
  sv12 <- rnorm(500, 0.2, 0.05)
  fake <- seq(0, 4, length.out = 25)

  a <- sim_coef_vec(sv1, sv12, fake, 0.95, multiplier = 2)
  b <- sim_coef_grad(cbind(sv1, sv12), cbind(1, 2 * fake), fake, 0.95)
  expect_equal(a$coef1, b$coef1, tolerance = 1e-12)
})

test_that("detect_nonlinear is FALSE for a plain interaction, TRUE otherwise", {
  expect_false(detect_nonlinear(terms(lm(mpg ~ wt * cyl, mtcars)), "wt", "cyl"))
  expect_true(detect_nonlinear(terms(lm(mpg ~ wt * (cyl + I(cyl^2)), mtcars)), "wt", "cyl"))
  expect_true(detect_nonlinear(terms(lm(mpg ~ wt * splines::ns(hp, 3), mtcars)), "wt", "hp"))
})

test_that("polynomial-in-moderator effect matches the analytic gradient", {
  m <- lm(mpg ~ wt * (cyl + I(cyl^2)), data = mtcars)
  set.seed(3)
  d <- interplot(m, "wt", "cyl", plot = FALSE)$df_coef
  cf <- coef(m)
  z <- d$wt # column 1 holds the cyl sequence
  expected <- cf["wt"] + cf["wt:cyl"] * z + cf["wt:I(cyl^2)"] * z^2
  expect_lt(max(abs(d$coef - unname(expected))), 0.2)
})

test_that("spline-in-moderator effect matches a finite difference of predict()", {
  m <- lm(mpg ~ wt * splines::ns(hp, 3), data = mtcars)
  set.seed(4)
  d <- interplot(m, "wt", "hp", plot = FALSE)$df_coef
  z <- d$wt
  h <- 1e-4
  wt0 <- mean(mtcars$wt)
  fd <- vapply(z, function(zz) {
    (predict(m, data.frame(wt = wt0 + h, hp = zz)) -
       predict(m, data.frame(wt = wt0, hp = zz))) / h
  }, numeric(1))
  expect_lt(max(abs(d$coef - fd)), 0.35)
})

test_that("a nonlinear model still returns a ggplot", {
  m <- lm(mpg ~ wt * (cyl + I(cyl^2)), data = mtcars)
  set.seed(5)
  expect_s3_class(interplot(m, "wt", "cyl"), "ggplot")
})
