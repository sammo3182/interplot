# Johnson-Neyman interval + layer.

test_that("jn_interval returns bounds and a recognized significance pattern", {
  m <- lm(mpg ~ wt * cyl, data = mtcars)
  jn <- jn_interval(m, "cyl", "wt")
  expect_s3_class(jn, "jn_interval")
  expect_true(jn$sig_pattern %in% c("always", "never", "between", "outside", "below", "above"))
  expect_length(jn$var2_range, 2)
})

test_that("jn_layer shading does not widen the x-axis beyond the data range", {
  m <- lm(mpg ~ wt * cyl, data = mtcars)
  jn <- jn_interval(m, "cyl", "wt")
  # This example has a JN bound (~14) far outside the data range (~[1.5, 5.4]);
  # the shaded non-significant region must be clipped to the data range so the
  # overlay does not stretch the axis (regression guard for that bug).
  p <- interplot(m, "cyl", "wt") + jn_layer(jn)
  xr <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$x.range
  # allow ggplot's default 5% expansion beyond the data range
  span <- diff(jn$var2_range)
  expect_lte(xr[2], jn$var2_range[2] + 0.1 * span)
  expect_gte(xr[1], jn$var2_range[1] - 0.1 * span)
})
