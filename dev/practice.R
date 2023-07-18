pacman::p_load(ggplot2, lme4, dplyr, purrr)

library(interplot)


m_cyl <- glm(am ~ wt * vs, data = mtcars,family = binomial(link = "logit"))
summary(m_cyl)


interplot(m_cyl, var1 = "wt",var2 = "vs",
                    predPro = TRUE,
                    var2_vals = c(0,1))
interplot(m_cyl, var1 = "vs",var2 = "wt",
                    predPro = TRUE,
                    var2_vals = c(min(mtcars$wt),max(mtcars$wt)))


m <- m_cyl
var1 <- "wt"
var2 <- "vs"
plot = TRUE
steps = NULL
ci = .95
adjCI = FALSE
hist = FALSE
var2_dt = NA
predPro = TRUE
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
facet_labs = NULL