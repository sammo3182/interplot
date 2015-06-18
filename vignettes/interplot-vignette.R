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
interplot(m = m_cyl, var1 = "cyl", var2 = "wt", steps = 5)

## ----fig.width = 5-------------------------------------------------------
interplot(m = m_cyl, var1 = "cyl", var2 = "wt", point = T)

## ----fig.width = 5-------------------------------------------------------
interplot(m = m_cyl, var1 = "cyl", var2 = "wt",
          xlab = "Weight", ylab = "Number of Cylinders")

## ----fig.width = 5-------------------------------------------------------
interplot(m = m_cyl, var1 = "cyl", var2 = "wt",
          xlab = "Weight", ylab = "Number of Cylinders") +
  ggtitle("The Conditional Effect of Weight
          on the Influence of Cylinder Number") + 
     theme(plot.title = element_text(face="italic"))  # adding a italic, two-line title

interplot(m = m_cyl, var1 = "cyl", var2 = "wt",
          xlab = "Weight", ylab = "Number of Cylinders") +
  geom_hline(yintercept = 0, color = "red")


## ----fig.width = 5.5-----------------------------------------------------
fake <- rnorm(100, 0, 1)
coef1 <- fake * sample(.5:2.5, 100, replace = T)
lb <- coef1 - .5
ub <- coef1 + .5

df_fake <- data.frame(cbind(fake, coef1, lb, ub))


interplot(df_fake)


