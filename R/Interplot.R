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
#' @import  arm
#' @import  abind
#' @import  ggplot2
#' 
#' @examples
#' # Create continuous, dummy, missing data, and group level variables.
#' 
#' group<-seq(1, 50, 1)
#' z<-rnorm(50, 1, 1) 
#' u<-rnorm(50, 0, 3)
#' df<-data.frame(group=rep(group, 50), z=rep(z,50), u=rep(u,50))

#' df$x1<-rnorm(2500, 3, 1)+0.1*(group-1)
#' df$d<-rbinom(2500, 1, 0.2)
#' 
#' df$x2<-sample(1:10, 2500, TRUE)
#' df$x2.miss <- df$x2
#' df$x2.miss[df$group < 5 & df$x1 < 4] <- NA
#' df$e<-rnorm(2500, 0, 2)
#' df$y<-2 - df$x1 + 0.3*df$x2 + 0.5*df$z + df$u + df$e
#'                  
#' # Apply the interplot to different regressions
#' library(Interplot)
#' 
#' ## 1. OLS
#' m1<-lm(y~x1+x2+d+z+x1:z, data = df)
#' interplot(m1, "x1", "z")
#' 
#' ## 2. Logit with two interaction terms (the second term is of interest)
#' m2<-glm(d~y+x1+x2+z+x1:z+y:z, family=binomial(link="logit"), data = df)
#' interplot(m2, "y", "z")
#' 
#' ## 3. Multilevel
#' library(lme4)
#' m3<-lmer(y~x1+x2+d+z+x1:z+(1|group), data = df)
#' interplot(m3, "x1","z")
#' 
#' ## 4. Multiple Imputed Data
#' library(Amelia)
#' m.imp <- amelia(df, idvars = c("y", "x2")) 
#' 
#' m4 <- lapply(m.imp$imputations, function(i) lm(y ~ x1 + x2.miss + d + x2.miss*z, data = i))
#' interplot(m4, "x2.miss","z") 
#' 
#' @export




interplot <- function(m, ...) {

  
  if (class(m)=="list"){
    if(class(m[[1]]) == "lmerMod"){class(m) <- "mlmmi"}
    if(class(m[[1]]) == "glmerMod"){class(m) <- "gmlmmi"}
    if(class(m[[1]]) == "lm"){class(m) <- "lmmi"}
    if(class(m[[1]]) == "glm"){class(m) <- "glmmi"}
  }
  if (class(m) == "data.frame") class(m) <- "plot"
  
  
  UseMethod("interplot", m)
  }
