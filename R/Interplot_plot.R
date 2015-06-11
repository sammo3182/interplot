#' Visualize Numeric Statistics of An Interaction
#' 
#' Graph based on the data frame of statistics about the conditional effect of an interaciton.
#' 
#' @param m a data.frame recording the statistics of the changes caused by the interaction. The data.frame should includes four columns:
#' \itemize{
#'    \item fake: the sequence of \code{var1} (the item whose effect will be conditioned on in the interaction);
#'    \item coef1: the point estimates of the coefficient of \code{var1} at each break point.
#'    \item ub: the upper bound of the simulated 95\% CI.
#'    \item lb: the lower bound of the simulated 95\% CI.
#' }
#' @param ylab set the label of the y axis.
#' @param xlab set the label of the x axis.
#' @param steps Desired length of the sequence. A non-negative number, which for seq and seq.int will be rounded up if fractional. The default is 100 or the unique categories in the \code{var2} (when it is less than 100. Also see \code{\link{unique}}).
#' @param point A logical value determining the format of plot. The function produces a point plot when it is \code{TRUE}; otherwise, the fucntion produces a line plot. Both plots have 95\% confidential intervals. The default is \code{FAULSE}.
#' @param ... additional arguments from \code{\link[ggplot2]{ggplot}}. 
#' 
#' @details \code{interplot.plot} is a S3 method from the \code{interplot}. It can graph the interaction effects based on a statistic data.frame recording the effects in a numeric way.
#' 
#' Because the output function is based on \code{\link[ggplot2]{ggplot}}, any additional arguments and layers supported by \code{ggplot2} can be added with the \code{+}. 
#' 
#' @return The function returns a \code{ggplot} object.
#' 
#' @import  ggplot2
#' 
#' @export

## S3 method for class 'data.frame'
interplot.plot <- function(m, ylab = NULL, xlab = NULL, steps = NULL, point = F, ...) {
  if(is.null(steps)) steps <- nrow(m)
  levels <- sort(unique(m$fake))
  
  if (steps <= 10 | point == T) {
    coef.plot <- ggplot(m, aes(x = fake, y = coef1)) + geom_point() + 
      geom_errorbar(aes(ymin = lb, ymax = ub), width = 0) + 
      scale_x_continuous(breaks = levels) + 
      theme_bw() + ylab(ylab) + xlab(xlab)
  } else {
    coef.plot <- ggplot(m, aes(x = fake, y = coef1)) + geom_line() + 
      geom_ribbon(aes(ymin = lb, ymax = ub), alpha = 0.5) + theme_bw() + 
      ylab(ylab) + xlab(xlab)
  }
  return(coef.plot)
} 
