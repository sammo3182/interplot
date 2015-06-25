#' Plot Conditional Coefficients in Models with Interaction Terms
#' 
#' Graph based on the data frame of statistics about the conditional effect of an interaciton.
#' 
#' @param m A model object including an interaction term, or, alternately, a data frame recording conditional coefficients. This data frame should includes four columns:
#' \itemize{
#'    \item fake: The sequence of \code{var1} (the item whose effect will be conditioned on in the interaction);
#'    \item coef1: The point estimates of the coefficient of \code{var1} at each break point.
#'    \item ub: The upper bound of the simulated 95\% CI.
#'    \item lb: The lower bound of the simulated 95\% CI.
#' }
#' @param var1 The name (as a string) of the variable of interest in the interaction term; its conditional coefficient estimates will be plotted.
#' @param var2 The name (as a string) of the other variable in the interaction term.
#' @param plot A logical value indicating whether the output is a plot or a dataframe including the conditional coefficient estimates of var1, their upper and lower bounds, and the corresponding values of var2.
#' @param point A logical value determining the format of plot. By default, the function produces a line plot when var2 takes on ten or more distinct values and a point (dot-and-whisker) plot otherwise; option TRUE forces a point plot.
#' @param sims Number of independent simulation draws used to calculate upper and lower bounds of coefficient estimates: lower values run faster; higher values produce smoother curves.
#' @param xmin A numerical value indicating the minimum value shown of x shown in the graph. Rarely used.
#' @param xmax A numerical value indicating the maximum value shown of x shown in the graph. Rarely used.
#' 
#' @details \code{interplot.plot} is a S3 method from the \code{interplot}. It generates plots of conditional coefficients.
#' 
#' Because the output function is based on \code{\link[ggplot2]{ggplot}}, any additional arguments and layers supported by \code{ggplot2} can be added with the \code{+}. 
#' 
#' @return The function returns a \code{ggplot} object.
#' 
#' @import  ggplot2
#' 
#' @export

## S3 method for class 'data.frame'
interplot.plot <- function(m, var1, var2, plot = TRUE, point = FALSE, sims = 5000,
                           xmin = NA, xmax = NA) {
  steps <- nrow(m)
  levels <- sort(unique(m$fake))
  
  if (steps < 10 | point == T) {
    coef.plot <- ggplot(m, aes_string(x = 'fake', y = 'coef1')) + geom_point() + 
      geom_errorbar(aes_string(ymin = 'lb', ymax = 'ub'), width = 0) + 
      scale_x_continuous(breaks = levels) + 
      ylab(NULL) + xlab(NULL)
  } else {
    coef.plot <- ggplot(m, aes_string(x = 'fake', y = 'coef1')) + geom_line() + 
      geom_ribbon(aes_string(ymin = 'lb', ymax = 'ub'), alpha = 0.5) + 
      ylab(NULL) + xlab(NULL)
  }
  return(coef.plot)
} 
