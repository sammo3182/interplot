interplot.lmerMod(gm1, var1 = "period", var2 = "herd", 
                  stats_cp = "ci", hist = TRUE)

pacman::p_load(ggplot2, lme4, dplyr, purrr, merTools)

df_mtcars <- mutate(mtcars, gear = as.factor(gear), carb = as.ordered(carb))
df_cbpp <- mutate(cbpp, herd = as.numeric(herd), period)

m_num <- lm(mpg ~ wt + hp, data = mtcars)
m_mlm <- glmer(incidence ~ size * herd + (1 | period), data = df_cbpp)

gm1 <- glmer(cbind(incidence, size - incidence) ~ period * herd + (1 | period), data = df_cbpp, family = binomial)

m <- m_mlm
var1 = "size"
var2 = "herd"
plot = TRUE
steps = NULL
ci = .95
adjCI = FALSE
hist = FALSE
var2_dt = NA
predPro = FALSE
var2_vals = NULL
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
facet_labs = NULL

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
  aPlot <- aPlot + facet_grid(. ~ value)
}

aPlot


