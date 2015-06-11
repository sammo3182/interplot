devtools::install_github("sammo3182/Interplot") # install most recent version
library(Interplot)

data(mtcars)

m1 <- lm(formula = mpg ~ wt + cyl + wt:cyl, data = mtcars)
summary(m1)

# Coefficient for number of cylinders on mileage by weight (in thousands of lbs)
interplot(m1, "cyl", "wt")  # as cars get heavier, 
                            # the magnitude of the negative relationship
                            # between number of cylinders and mileage declines and,
                            # at ~3500lbs, loses statistical significance

# Coefficient for weight (in thousands of lbs) on mileage by number of cylinders
interplot(m1, "wt", "cyl", steps=3) # weight is always estimated to have a
                                    # statistically significant negative relationship
                                    # to mileage, but this relationship is weaker
                                    # for cars with more cylinders

