#' Graph Interaction Effects in Multilevel Models with Imputed Data
#' 
#' \code{interplot.default} is a method to graph interaction effects from the results of multilevel linear and general linear regression models with imputed data.
#' 
#' @param m a model object including the interaction of interest.
#' @param var1 The target variable in the interaction term of interest, whose coefficient changes is intended to plot.
#' @param var2 The changing variable in the interaction term of interest, along with the changes of which the changes of the coefficient of the other variable is shown in the plot.
#' @param xlab A character variable to set the title for the x axis.
#' @param ylab A character variable to set the title for the y axis.
#' @param labels A logical value to deside whether using the variable labels.
#' @param seed An arbitrary numeric value. The default value is 324.
#' @param sims Number of independent simulation draws to create.
#' @param steps Desired length of the sequence. A non-negative number, which for seq and seq.int will be rounded up if fractional.
#' @param xmin a numerical value deciding the minimum value shown of x shown in the graph.
#' @param xmax a numerical value deciding the maximum value shown of x shown in the graph.
#' @param plot A logical value to deside the output is a plot or a list of the coefficient, upper and lower bound of var2.
#' @param ... additional arguments affecting the output of the plot.
#' 
#' @details \code{interplot} is a S3 method from the \code{interplot}. It can visualize the changes in the coefficient of one term in a two-way interaction conditioned by the other term. This function can work on interactions produced by \code{lmer4} package.
#' 
#' Because the output function is based on \code{\link[ggplot2]{ggplot}}, any additional arguments and layers supported by \code{ggplot2} can be added with the \code{+}. 
#' 
#' @return The function returns a \code{ggplot} object.
#' 
#' @import  arm
#' @import  abind
#' @import  ggplot2
#' 
#' 
#' @export

#Coding function for mlm, mi objects 
interplot.mlmmi <- function(m, var1, var2, xlab=NULL, ylab=NULL, 
                            seed=324, sims=1000, steps=100, xmin=NA,
                            xmax=NA, labels=NULL, plot=TRUE){
  set.seed(seed)
  
  class(m) <- "list"
  m.list <- m
  m <- m.list[[1]]
  m.class <- class(m)
  m.sims.list <- lapply(m.list, function(i) arm::sim(i, sims))
  m.sims <- m.sims.list[[1]]
  
  for(i in 2:length(m.sims.list)) {
    m.sims@fixef <- rbind(m.sims@fixef, m.sims.list[[i]]@fixef)
    m.sims@ranef[[1]] <- abind(m.sims@ranef[[1]], m.sims.list[[i]]@ranef[[1]], along=1)
  }
  
  ifelse(var1==var2, var12 <- paste0("I(", var1, "^2)"), var12 <- paste0(var2,":",var1))
  
  if (!var12 %in% unlist(dimnames(m@pp$X)[2])) var12 <- paste0(var1,":",var2)
  if (!var12 %in% unlist(dimnames(m@pp$X)[2])) stop(paste("Model does not include the interaction of",var1 ,"and",var2, "."))
  if (is.na(xmin)) xmin <- min(m@frame[var2], na.rm=T)
  if (is.na(xmax)) xmax <- max(m@frame[var2], na.rm=T)        
  coef <- data.frame(fake = seq(xmin, xmax, length.out=steps), coef1 = NA, ub = NA, lb = NA)
  
  for(i in 1:steps) {   
    coef$coef1[i] <- mean(m.sims@fixef[,match(var1, unlist(dimnames(m@pp$X)[2]))] + 
                            coef$fake[i]*m.sims@fixef[,match(var12, unlist(dimnames(m@pp$X)[2]))])
    coef$ub[i] <- quantile(m.sims@fixef[,match(var1, unlist(dimnames(m@pp$X)[2]))] + 
                             coef$fake[i]*m.sims@fixef[,match(var12, unlist(dimnames(m@pp$X)[2]))], .975)
    coef$lb[i] <- quantile(m.sims@fixef[,match(var1, unlist(dimnames(m@pp$X)[2]))] + 
                             coef$fake[i]*m.sims@fixef[,match(var12, unlist(dimnames(m@pp$X)[2]))], .025)  
  }   
  
  if(plot==TRUE) {
    interplot.plot(m = coef, steps = steps, ylab = ylab, xlab = xlab)
  } else {
    names(coef) <- c(var2, "coef", "ub", "lb")
    return(coef)
  }
}


interplot.gmlmmi <- function(m, var1, var2, xlab=NULL, ylab=NULL, 
                             seed=324, sims=1000, steps=100, xmin=NA,
                             xmax=NA, labels=NULL, plot=TRUE){
  set.seed(seed)
  
  class(m) <- "list"
  m.list <- m
  m <- m.list[[1]]
  m.class <- class(m)
  m.sims.list <- lapply(m.list, function(i) arm::sim(i, sims))
  m.sims <- m.sims.list[[1]]
  
  for(i in 2:length(m.sims.list)) {
    m.sims@fixef <- rbind(m.sims@fixef, m.sims.list[[i]]@fixef)
    m.sims@ranef[[1]] <- abind(m.sims@ranef[[1]], m.sims.list[[i]]@ranef[[1]], along=1)
  }
  
  ifelse(var1==var2, var12 <- paste0("I(", var1, "^2)"), var12 <- paste0(var2,":",var1))
  
  if (!var12 %in% unlist(dimnames(m@pp$X)[2])) var12 <- paste0(var1,":",var2)
  if (!var12 %in% unlist(dimnames(m@pp$X)[2])) stop(paste("Model does not include the interaction of",var1 ,"and",var2, "."))
  if (is.na(xmin)) xmin <- min(m@frame[var2], na.rm=T)
  if (is.na(xmax)) xmax <- max(m@frame[var2], na.rm=T)        
  coef <- data.frame(fake = seq(xmin, xmax, length.out=steps), coef1 = NA, ub = NA, lb = NA)
  
  for(i in 1:steps) {   
    coef$coef1[i] <- mean(m.sims@fixef[,match(var1, unlist(dimnames(m@pp$X)[2]))] + 
                            coef$fake[i]*m.sims@fixef[,match(var12, unlist(dimnames(m@pp$X)[2]))])
    coef$ub[i] <- quantile(m.sims@fixef[,match(var1, unlist(dimnames(m@pp$X)[2]))] + 
                             coef$fake[i]*m.sims@fixef[,match(var12, unlist(dimnames(m@pp$X)[2]))], .975)
    coef$lb[i] <- quantile(m.sims@fixef[,match(var1, unlist(dimnames(m@pp$X)[2]))] + 
                             coef$fake[i]*m.sims@fixef[,match(var12, unlist(dimnames(m@pp$X)[2]))], .025)  
  }   
  
  if(plot==TRUE) {
    interplot.plot(m = coef, steps = steps, ylab = ylab, xlab = xlab)
  } else {
    names(coef) <- c(var2, "coef", "ub", "lb")
    return(coef)
  }
}
