#' ---
#' title: "Crop Analysis Q3 2013"
#' author: "Yue Hu"
#' ---


library(interplot)

#' Toy models
m_cyl1 <- lm(mpg ~ wt * cyl, data = mtcars[1:16,])
m_cyl2 <- lm(mpg ~ wt * cyl, data = mtcars[17:32,])


#' Build the data frames and categorize them manually
#' 
df_outcome1 <- interplot(m = m_cyl1, var1 = 'cyl', var2 = 'wt', plot = FALSE)
df_outcome1$value <- "Category 1" # variable name has to be "value"

df_outcome2 <- interplot(m = m_cyl2, var1 = 'cyl', var2 = 'wt', plot = FALSE)
df_outcome2$value <- "Category 2"

#' combine the data frames
df_outcome <- rbind(df_outcome1, df_outcome2)
df_outcome$value <- as.factor(df_outcome$value) # required step
names(df_outcome) <- c("fake", "coef1", "lb", "ub", "value") # variables have to be named in this way
df_outcome

#' Let `interplot` presents the effect by category
#' 
interplot(df_outcome, predPro = TRUE)

rmarkdown::render("dev/plot-by-category.R")
