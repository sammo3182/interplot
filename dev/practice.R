interplot(m = m_factor, var2 = "gear", var1 = "wt")

pacman::p_load(ggplot2, lme4, dplyr, purrr)

df_mtcars <- mutate(mtcars, gear = as.factor(gear), carb = as.ordered(carb))
m_factor <- lm(mpg ~ wt * gear, data = df_mtcars)

m <- m_factor
var2 = "gear"
var1 = "wt"
plot = TRUE
steps = NULL
ci = .95
adjCI = FALSE
hist = FALSE
var2_dt = NA
predPro = FALSE
var2_vals = c(0,1)
point = FALSE
sims = 1000
xmin = NA
xmax = NA
ercolor = NA
esize = 0.5
ralpha = 0.5
rfill = "grey70"
stats_cp = "none"
txt_caption = NULL
facet_labs = c("4-speed", "5-speed")

aPlot <- interplot.plot(
  m = coef_df,
  hist = hist,
  steps = steps,
  var2_dt = var2_dt,
  predPro = predPro,
  point = point,
  ercolor = ercolor,
  esize = esize,
  ralpha = ralpha,
  rfill = rfill,
  stats_cp = stats_cp,
  txt_caption = txt_caption,
  ci_diff = ci_diff,
  ks_diff = ks_diff
)

if (factor_v1 | factor_v2) {
  if (is.null(facet_labs)) facet_labs <- unique(coef_df$value)
  
  coef_df$value <- factor(coef_df$value, labels = facet_labs)
  
  aPlot <- aPlot + facet_grid(. ~ value)
}

aPlot
