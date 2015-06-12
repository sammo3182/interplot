#' Plot Interactive Effects
#' 
#' \code{interplot} is a generic function to produce plot of the conditional graph of one variable in an two-way interaction conditioned on the other variable from the results of various model fitting functions. The function invokes particular \code{methods} which depend on the \code{\link{class}} of the first argument. 
#' 
#' @param m a model object including the interaction of interest.
#' @param ... additional arguments affecting the output of the plot.
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
#' ggtitle("The Conditional Effect of Weight \n 
#' on the Influence of Cylinder Number") + 
#' theme(plot.title = element_text(face="italic"))
#' 
#' 
#' # Plot interactions with a categorical conditioning variable
#' interplot(m = m_cyl, var1 = "wt", var2 = "cyl")
#' 
#' @export




interplot <- function(m, ...) {
    
    
    if (class(m) == "list") {
        if (class(m[[1]]) == "lmerMod") {
            class(m) <- "mlmmi"
        }
        if (class(m[[1]]) == "glmerMod") {
            class(m) <- "gmlmmi"
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
