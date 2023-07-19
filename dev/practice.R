interplot(m = m_factor, 
          var1 = "gear", var2 = "wt", 
          facet_labs = c("try", "out"))

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
stats_cp = "ci"
txt_caption = NULL
facet_labs = NULL

test_cp <-
  if (is.list(ci_diff)) {
  map2(ci_diff, levels(coef_df$value), \(aCI, aLevel){
    paste0(aLevel,
           ", CI(Max - Min): [",
           round(aCI[1], digits = 3),
           ", ",
           round(aCI[2], digits = 3),
           "]")
  }) |> 
  list_c()
} else {
  paste0("CI(Max - Min): [",
         round(ci_diff[1], digits = 3),
         ", ",
         round(ci_diff[2], digits = 3),
         "]")
}


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

test_cp <- map2(ci_diff, levels(coef_df$value), \(aCI, aLevel){
  paste0(aLevel,
         "\n CI(Max - Min): [",
         round(aCI[1], digits = 3),
         ", ",
         round(aCI[2], digits = 3),
         "]")
}) |> 
  list_c()

aPlot + 
  facet_grid(. ~ value, 
             labeller = labeller(test_cp))
