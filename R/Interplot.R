#' INTERPLOT: Plot Interactive Effects
#' 
#' interplot is the basic plotting function in the interplot package. It is a convenient function for creating graphs describing how the coefficient of one variable in a two-way interaction term will change along with the changes of the other variable.
#' @param m A R object storing the result of a regression including at least one interaction term.
#' @param var1 The target variable in the interaction term of interest, whose coefficient changes is intended to plot.
#' @param var2 The changing variable in the interaction term of interest, along with the changes of which the changes of the coefficient of the other variable is shown in the plot.
#' @param xlab A title for the x axis.
#' @param ylab A title for the y axis.
#' @param labels A logical value to deside whether using the variable labels.
#' @param seed An arbitrary numeric value.
#' @param sims Number of independent simulation draws to create.
#' @param steps Desired length of the sequence. A non-negative number, which for seq and seq.int will be rounded up if fractional.
#' @param xmin a numerical value deciding the minimum value shown of x shown in the graph.
#' @param xmax a numerical value deciding the maximum value shown of x shown in the graph.
#' @param plot A logical value to deside the output is a plot or a list of the coefficient, upper and lower bound of var2.
#' @export
#' @examples
#' # Create continuous, dummy, and group level variables.
#' group<-seq(1, 50, 1)
#' z<-rnorm(50, 1, 1)
#' u<-rnorm(50, 0, 3)
#' 
#' df<-data.frame(group=rep(group, 50), z=rep(z,50), u=rep(u,50))
#' 
#' df$x<-rnorm(2500, 3, 1)+0.1*(group-1)
#' 
#' df$d<-rbinom(2500, 1, 0.2)
#' df$e<-rnorm(2500, 0, 2)
#' df$y<-2-df$x+0.5*df$z+df$u+df$e
#'                  
#' # Apply the interplot to different regressions
#' library(Interplot)
#' 
#' ## 1. OLS
#' m1<-lm(y~x+d+z+x:z)
#' interplot(m1, "x", "z")
#' 
#' ## 2. Logit with two interaction terms (the second term is of interest)
#' m2<-glm(d~y+x+z+x:z+y:z, family=binomial(link="logit"))
#' interplot(m2, "y", "z")
#' 
#' ## 3. Multilevel
#' m3<-lmer(y~x+d+z+x:z+(1|m))
#' interplot(m3, "x","z")




interplot <- function(m, var1, var2, xlab=NULL, ylab=NULL, labels = NULL,
                      seed=313, sims=1000, steps=100, xmin=NA,
                      xmax=NA, plot=TRUE) {
  require(arm)
  require(ggplot2)
  require(abind)
  
  set.seed(seed)
  if (class(m)=="list") {
    m.list <- m
    m <- m.list[[1]]
    m.class <- class(m)
    m.sims.list <- lapply(m.list, function(i) arm::sim(i, sims))
    m.sims <- m.sims.list[[1]]
    if (m.class=="lmerMod" | m.class=="glmerMod") {
      for(i in 2:length(m.sims.list)) {
        m.sims@fixef <- rbind(m.sims@fixef, m.sims.list[[i]]@fixef)
        m.sims@ranef[[1]] <- abind(m.sims@ranef[[1]], m.sims.list[[i]]@ranef[[1]], along=1)
      }
    } else {
      stop(paste("Multiply imputed flat models not implemented yet."))
    }
  } else {
    m.class <- class(m)
    m.sims <- arm::sim(m, sims)
  }
  if(var1==var2) var12 <- paste0("I(", var1, "^2)") else var12 <- paste0(var2,":",var1)
  if(m.class!="lmerMod" & m.class!="glmerMod"){
    if (!var12 %in% names(m$coef)) var12 <- paste0(var1,":",var2)
    if (!var12 %in% names(m$coef)) stop(paste("Model does not include the interaction of",var1 ,"and",var2, "."))
    if (is.na(xmin)) xmin <- min(m$model[var2], na.rm=T)
    if (is.na(xmax)) xmax <- max(m$model[var2], na.rm=T)
    coef <- data.frame(fake = seq(xmin, xmax, length.out=steps), coef1 = NA, ub = NA, lb = NA)
    
    for(i in 1:steps) {    
      coef$coef1[i] <- mean(m.sims@coef[,match(var1, names(m$coef))] + 
                              coef$fake[i]*m.sims@coef[,match(var12, names(m$coef))])
      coef$ub[i] <- quantile(m.sims@coef[,match(var1, names(m$coef))] + 
                               coef$fake[i]*m.sims@coef[,match(var12, names(m$coef))], .975)
      coef$lb[i] <- quantile(m.sims@coef[,match(var1, names(m$coef))] + 
                               coef$fake[i]*m.sims@coef[,match(var12, names(m$coef))], .025)    
    }
  } else {
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
  }
  if (plot==TRUE) {
    if(steps>10) {
      coef.plot <- ggplot(coef, aes(x = fake, y = coef1)) + 
        geom_line() + geom_ribbon(aes(ymin=lb, ymax=ub), alpha=.5) +
        theme_bw() + ylab(ylab) + xlab(xlab)
    } else {
      if (is.null(labels)) {
        coef.plot <- ggplot(coef, aes(x = fake, y = coef1)) + 
          geom_point() + geom_errorbar(aes(ymin=lb, ymax=ub), width=0) +
          scale_x_continuous(breaks = seq(min(coef$fake), max(coef$fake), length.out=steps)) +
          theme_bw() + ylab(ylab) + xlab(xlab)
      } else {
        coef.plot <- ggplot(coef, aes(x = fake, y = coef1)) + 
          geom_point() + geom_errorbar(aes(ymin=lb, ymax=ub), width=0) +
          scale_x_continuous(breaks = seq(min(coef$fake), max(coef$fake), length.out=steps),
                             labels = labels) +
          theme_bw() + ylab(ylab) + xlab(xlab)
      } 
    }
    return(coef.plot)
  } else {
    names(coef) <- c(var2, "coef", "ub", "lb")
    return(coef)
  }
}