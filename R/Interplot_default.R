#' Graph Interaction Effects in Regular Models
#' 
#' \code{interplot.default} is a method to graph interaction effects from the results of linear and general linear regression models. 
#' 
#' @param m A model object including the interaction of interest.
#' @param var1 A charecter value showing the target variable in the interaction term of interest, whose coefficient changes is intended to plot.
#' @param var2 A charecter value showing the changing variable in the interaction term of interest, along with the changes of which the changes of the coefficient of the other variable is shown in the plot.
#' @param xlab A character variable to set the title for the x axis.
#' @param ylab A character variable to set the title for the y axis.
#' @param labels A logical value to deside whether using the variable labels.
#' @param seed An arbitrary numeric value. The default value is 324.
#' @param sims Number of independent simulation draws to create.
#' @param steps Desired length of the sequence. A non-negative number, which for seq and seq.int will be rounded up if fractional. The default is 100 or the unique categories in the \code{var2} (when it is less than 100. Also see \code{\link{unique}}).
#' @param xmin A numerical value deciding the minimum value shown of x shown in the graph.
#' @param xmax A numerical value deciding the maximum value shown of x shown in the graph.
#' @param plot A logical value to deside the output is a plot or a list of the coefficient, upper and lower bound of var2.
#' @param point A logical value determining the format of plot. The function produces a point plot when it is \code{TRUE}; otherwise, the fucntion produces a line plot. Both plots have 95\% confidential intervals. The default is \code{FAULSE}.
#' 
#' @details \code{interplot} is a S3 method from the \code{interplot}. It can visualize the changes in the coefficient of one term in a two-way interaction conditioned by the other term. This function can work on interactions in two classes of objects:
#' \itemize{
#'   \item Ordinary linear models (object class: \code{lm});
#'   \item Generalized linear models (object class: \code{glm}).
#'   }
#' 
#' The examples below illustrate how methods invoked by this generic deal with different type of objects.
#' 
#' Because the output function is based on \code{\link[ggplot2]{ggplot}}, any additional arguments and layers supported by \code{ggplot2} can be added with the \code{+}. 
#' 
#' @return The function returns a \code{ggplot} object.
#' 
#' @import  abind
#' @import  arm
#' @import  ggplot2
#' 
#' 
#' 
#' @export

# S3 method for class 'lm' and 'glm'
interplot.default <- function(m, var1, var2, xlab = NULL, ylab = NULL, seed = 324, 
    sims = 1000, steps = NULL, xmin = NA, xmax = NA, labels = NULL, plot = TRUE, point = FALSE) {
    set.seed(seed)
    
    m.class <- class(m)
    m.sims <- arm::sim(m, sims)
    
    ifelse(var1 == var2, var12 <- paste0("I(", var1, "^2)"), var12 <- paste0(var2, 
        ":", var1))
    
    if (!var12 %in% names(m$coef)) 
        var12 <- paste0(var1, ":", var2)
    if (!var12 %in% names(m$coef)) 
        stop(paste("Model does not include the interaction of", var1, "and", 
            var2, "."))
    if (is.na(xmin)) 
        xmin <- min(m$model[var2], na.rm = T)
    if (is.na(xmax)) 
        xmax <- max(m$model[var2], na.rm = T)
    
    if (is.null(steps)) {
        steps <- eval(parse(text = paste0("length(unique(na.omit(m$model$",var2,")))")))
        if (steps > 100) steps <- 100 # avoid redundant calculation
    }
    
    
    coef <- data.frame(fake = seq(xmin, xmax, length.out = steps), coef1 = NA, 
        ub = NA, lb = NA)
    
    for (i in 1:steps) {
        coef$coef1[i] <- mean(m.sims@coef[, match(var1, names(m$coef))] + 
            coef$fake[i] * m.sims@coef[, match(var12, names(m$coef))])
        coef$ub[i] <- quantile(m.sims@coef[, match(var1, names(m$coef))] + 
            coef$fake[i] * m.sims@coef[, match(var12, names(m$coef))], 0.975)
        coef$lb[i] <- quantile(m.sims@coef[, match(var1, names(m$coef))] + 
            coef$fake[i] * m.sims@coef[, match(var12, names(m$coef))], 0.025)
    }
    
    if (plot == TRUE) {
        interplot.plot(m = coef, ylab = ylab, xlab = xlab, point = point)
    } else {
        names(coef) <- c(var2, "coef", "ub", "lb")
        return(coef)
    }
} 
