#' Plot Interactive Effects
#' 
#' \code{interplot} is a generic function to produce plot of the conditional graph of one variable in an two-way interaction conditioned on the other variable from the results of various model fitting functions. The function invokes particular \code{methods} which depend on the \code{\link{class}} of the first argument. 
#' 
#' @param m A model object including the interaction of interest, or a data.frame recording the statistics of the changes caused by the interaction.
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
#' @details \code{interplot} visualizes the changes in the coefficient of one term in a two-way interaction conditioned by the other term. In the current version, the function can work on interactions in four classes of objects:
#' \itemize{
#'   \item Ordinary linear models (object class: \code{lm});
#'   \item Generalized linear models (object class: \code{glm});
#'   \item Ordinary linear models with imputed data (object class: \code{list});
#'   \item Generalized linear models with imputed data (object class: \code{list}).
#' }
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
#' @examples
#' data(mtcars)
#' m_cyl <- lm(mpg ~ wt * cyl, data = mtcars)
#' library(interplot)
#' 
#' # Plot interactions with a continous conditioning variable
#' interplot(m = m_cyl, var1 = "cyl", var2 = "wt") +
#' xlab("Automobile Weight (thousands lbs)") +
#' ylab("Estimated Coefficient for Number of Cylinders") +
#' ggtitle("Estimated Coefficient of Engine Cylinders\non Mileage by Automobile Weight") +
#' theme(plot.title = element_text(face="bold"))
#' 
#' 
#' # Plot interactions with a categorical conditioning variable
#' interplot(m = m_cyl, var1 = "wt", var2 = "cyl") +
#' xlab("Number of Cylinders") +
#' ylab("Estimated Coefficient for Automobile Weight (thousands lbs)") +
#' ggtitle("Estimated Coefficient of Automobile Weight \non Mileage by Engine Cylinders") +
#' theme(plot.title = element_text(face="bold"))
#' 
#' @export




interplot <- function(m, var1, var2, xlab = NULL, ylab = NULL, seed = 324, 
                      sims = 1000, steps = NULL, xmin = NA, xmax = NA, labels = NULL, 
                      plot = TRUE, point = FALSE) {
    
    
    if (class(m) == "list") {
        if (class(m[[1]]) == "lmerMod") {
            class(m) <- "mlmmi"
        }
        if (class(m[[1]]) == "glmerMod") {
            class(m) <- "glmmi"
        }
        if (class(m[[1]]) == "lm") {
            class(m) <- "lmmi"
        }
        if (class(m[[1]]) == "glm") {
            class(m) <- "glmmi"
        }
    }
  
    if (class(m) == "data.frame") 
        class(m) <- "plot"
    
    
    UseMethod("interplot", m)
} 
