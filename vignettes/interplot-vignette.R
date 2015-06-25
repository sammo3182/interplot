## ------------------------------------------------------------------------
data(mtcars)  #load the data

## ------------------------------------------------------------------------

m_cyl <- lm(mpg ~ wt * cyl, data = mtcars)
summary(m_cyl)



## ----fig.width = 5-------------------------------------------------------
library(interplot)

interplot(m = m_cyl, var1 = "cyl", var2 = "wt")

## ----fig.width = 5-------------------------------------------------------
interplot(m = m_cyl, var1 = "wt", var2 = "cyl") +
  theme(axis.text.x  = element_text(angle=90))

## ----fig.width = 5-------------------------------------------------------
interplot(m = m_cyl, var1 = "cyl", var2 = "wt", point = T) +
  theme(axis.text.x  = element_text(angle=90))

## ----fig.width = 5-------------------------------------------------------
interplot(m = m_cyl, var1 = "cyl", var2 = "wt") +
  # adding the axis labels
  xlab("Weight") + ylab ("Number of Cylinders") + 
  # let the title to be italic and double-line
  ggtitle("The Conditional Effect of Weight
          on the Influence of Cylinder Number") + 
     theme(plot.title = element_text(face="italic"))  

interplot(m = m_cyl, var1 = "cyl", var2 = "wt") +
  xlab("Weight") + ylab ("Number of Cylinders") + 
  #adding a horizontal line at y = 0
  geom_hline(yintercept = 0, color = "red")


## ----fig.width = 5.5-----------------------------------------------------
fake <- rnorm(100, 0, 1)
coef1 <- fake * sample(.5:2.5, 100, replace = T)
lb <- coef1 - .5
ub <- coef1 + .5

df_fake <- data.frame(cbind(fake, coef1, lb, ub))


interplot(df_fake)


