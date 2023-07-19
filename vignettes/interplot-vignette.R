## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE,
  dpi = 300
)

## -----------------------------------------------------------------------------
data(mtcars)  #load the data

## -----------------------------------------------------------------------------
m_cyl <- lm(mpg ~ wt * cyl, data = mtcars)
summary(m_cyl)

## ----out.width="80%"----------------------------------------------------------
library(interplot)

interplot(m = m_cyl, var1 = "cyl", var2 = "wt")

## ----out.width="80%"----------------------------------------------------------
interplot(m = m_cyl, var1 = "wt", var2 = "cyl")

## ----out.width="80%"----------------------------------------------------------
interplot(m = m_cyl, var1 = "wt", var2 = "cyl", ci = .9, point = T)

## ----out.width="80%"----------------------------------------------------------
interplot(m = m_cyl, var1 = "cyl", var2 = "wt", point = T) +
  # changing the angle of x labels for a clearer vision
  theme(axis.text.x  = element_text(angle=90))

## ----out.width="80%"----------------------------------------------------------
interplot(m = m_cyl, var1 = "cyl", var2 = "wt") + 
  # Add labels for X and Y axes
    xlab("Automobile Weight (thousands lbs)") +
    ylab("Estimated Coefficient for\nNumber of Cylinders") +
  # Change the background
    theme_bw() +
  # Add the title
    ggtitle("Estimated Coefficient of Engine Cylinders \non Mileage by Automobile Weight") +
    theme(plot.title = element_text(face="bold")) +
  # Add a horizontal line at y = 0
    geom_hline(yintercept = 0, linetype = "dashed")

## ----out.width="80%"----------------------------------------------------------
interplot(m = m_cyl, var1 = "wt", var2 = "cyl", ercolor = "blue", esize = 1.5) +
  geom_point(size = 2, color = "red")

## ----out.width="80%"----------------------------------------------------------
m_wt <- lm(mpg ~ wt + I(wt^2), data = mtcars)

interplot(m = m_wt, var1 = "wt", var2 = "wt")

## ----out.width="80%"----------------------------------------------------------
mtcars$gear <- factor(mtcars$gear)
m_gear <- lm(mpg ~ gear * wt, data = mtcars)

interplot(m = m_gear, var1 = "wt", var2 = "gear")

## ----out.width="80%"----------------------------------------------------------
interplot(m = m_gear, var1 = "wt", var2 = "gear", facet_labs = c("4-speed", "5-speed"))

## ----out.width="80%"----------------------------------------------------------
interplot(m = m_cyl, var1 = "cyl", var2 = "wt", hist = TRUE) +
    geom_hline(yintercept = 0, linetype = "dashed")

## ----out.width="80%"----------------------------------------------------------
interplot(m = m_cyl, var1 = "cyl", var2 = "wt", hist = TRUE) +
  aes(color = "pink") + theme(legend.position="none") +  # geom_line(color = "pink") + 
  geom_hline(yintercept = 0, linetype = "dashed")

## -----------------------------------------------------------------------------
set.seed(313)

interplot(m = m_cyl, var1 = "cyl", var2 = "wt", stats_cp = "ci")

## -----------------------------------------------------------------------------
set.seed(313)

interplot(m = m_cyl, var1 = "cyl", var2 = "wt", stats_cp = "ci", 
          txt_caption = "\n Source: Motor Trend Car Road Tests 1973.")

## ----out.width="80%"----------------------------------------------------------
stdCI_plot <- interplot(m = m_wt, var1 = "wt", var2 = "wt", adjCI = FALSE) +
ggtitle("Marginal Effects with Standard CIs")
adjCI_plot <- interplot(m = m_wt, var1 = "wt", var2 = "wt", adjCI = TRUE) +
ggtitle("Marginal Effects with Adjusted CIs")

library(gridExtra)
grid.arrange(stdCI_plot, adjCI_plot, ncol = 2)

## ----out.width="80%"----------------------------------------------------------
pew1.w <- read.csv("pew1_w.csv")

m <- glm(formula=meritocracy~ginicnty+income_i+ginicnty:income_i+income_cnty+black_cnty+
                perc_bush04+pop_cnty+educ_i+age_i+gender_i+unemp_i+union_i+partyid_i+
                ideo_i+attend_i+survid2006+survid2007+survid2009,
              data=pew1.w,family=binomial(link="logit"))
              
plot_avg <- interplot(m, var1 = "ginicnty",var2 = "income_i", predPro = FALSE) + 
ggtitle("Average Conditional Effects")

plot_3val <- interplot(m, var1 = "ginicnty",var2 = "income_i", predPro = TRUE, var2_vals = c(min(pew1.w$income_i), max(pew1.w$income_i))) +
ggtitle("Conditional Predicted Probabilities for \nCitizens with Low and High Incomes") +
scale_colour_discrete(guide = guide_legend(title = "Income"), labels = c("Low", "High")) + 
scale_fill_discrete(guide = guide_legend(title = "Income"), labels = c("Low", "High")) +
theme(legend.position = c(0, .8), legend.justification = c(0, .5))

grid.arrange(plot_avg, plot_3val, ncol = 2)

## ----out.width="80%"----------------------------------------------------------
# Create a fake dataset of conditional effects
fake <- rnorm(100, 0, 1)
coef1 <- fake * sample(.5:2.5, 100, replace = T)
lb <- coef1 - .5
ub <- coef1 + .5

df_fake <- data.frame(cbind(fake, coef1, lb, ub))

# Use interplot directly with the dataset
interplot(df_fake)

## ----out.width="80%"----------------------------------------------------------
var2_fake <- fake
# Set `hist` to TRUE is required to superimpose a histogram.
interplot(df_fake, hist = TRUE, var2_dt = var2_fake)
## The ribbon and histogram do not fit. This is just an illustration

