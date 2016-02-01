## ------------------------------------------------------------------------
data(mtcars)  #load the data

## ------------------------------------------------------------------------

m_cyl <- lm(mpg ~ wt * cyl, data = mtcars)
summary(m_cyl)



## ----fig.width = 5-------------------------------------------------------
library(interplot)

interplot(m = m_cyl, var1 = "cyl", var2 = "wt")

## ----fig.width = 5-------------------------------------------------------
interplot(m = m_cyl, var1 = "wt", var2 = "cyl")

## ----fig.width = 5-------------------------------------------------------
interplot(m = m_cyl, var1 = "cyl", var2 = "wt", point = T) +
  # changing the angle of x labels for a clearer vision
  theme(axis.text.x  = element_text(angle=90))

## ----fig.width = 5-------------------------------------------------------
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

## ----fig.width = 5-------------------------------------------------------
mtcars$gear <- factor(mtcars$gear)
m_gear <- lm(mpg ~ gear * wt, data = mtcars)

interplot(m = m_gear, var1 = "wt", var2 = "gear")

## ----fig.width = 5.5-----------------------------------------------------
# Create a fake dataset of conditional effects
fake <- rnorm(100, 0, 1)
coef1 <- fake * sample(.5:2.5, 100, replace = T)
lb <- coef1 - .5
ub <- coef1 + .5

df_fake <- data.frame(cbind(fake, coef1, lb, ub))

# Use interplot directly with the dataset
interplot(df_fake)

