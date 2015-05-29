##Data Example###
#Simulated Data####
group<-seq(1, 50, 1)
 z<-rnorm(50, 1, 1)
 u<-rnorm(50, 0, 3)
 
 df<-data.frame(group=rep(group, 50), z=rep(z,50), u=rep(u,50))
 
 df$x<-rnorm(2500, 3, 1)+0.1*(group-1)
 
 df$d<-rbinom(2500, 1, 0.2)
 df$e<-rnorm(2500, 0, 2)
 df$y<-2-df$x+0.5*df$z+df$u+df$e
 
# Real Data ####
ipp3.full <- read.csv("E:/Dropbox_sync/Method/Data/IPP/ipp3full.csv")
 source("E:/Dropbox_sync/Dropbox/Instruction/Research paper/PoliticalDesirability/codes/polidesi.functions.R")

library(car)
library(MASS)
library(dplyr)
### Imputation ####
 ###By group ####
 ##1. find proper cutting variale
 ipp3.full$age.3s <- recode(ipp3.full$age, "3:4 = 3; 5:7 = 4")
 
 ipp3.full$edu.3s <- recode(ipp3.full$edu, "1:3 = 1; 4 = 2; 5:6 = 3")
 
 
 ipp3.full <- mutate(ipp3.full,
   muneffi = recode(a5_010, "8 = NA") - 1,
   frespee = recode(a1_006, "8 = NA") - 1,
   govresp = recode(a1_008, "8 = NA") - 1,
   govwork = recode(a6_003, "8 = NA"),
   relig = as.factor(b10), city = x1
 ) %>% dplyr::select(age:city, wt)
 
library(Amelia)

 ipp3.imp <- amelia(ipp3.full, idvars = c("relig")) 
 
m.lm <- lapply(ipp3.imp$imputations, function(i) lm(govwork ~ income +  muneffi * female, data = i))
 
interplot(m = m.lm, var1 = "muneffi", var2 = "female", xmin = .25, xmax = .75)

m <- m.lm
sims <- 10
seed <- 313
var1 <- muneffi

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
      for(i in 2:length(m.sims.list)) {
        m.sims@coef <- rbind(m.sims@coef, m.sims.list[[i]]@coef)
      }
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


## Create missing data
 group<-seq(1, 50, 1)
 z<-rnorm(50, 1, 1) 
 u<-rnorm(50, 0, 3)
 df<-data.frame(group=rep(group, 50), z=rep(z,50), u=rep(u,50))

 df$x1<-rnorm(2500, 3, 1)+0.1*(group-1)
 df$d<-rbinom(2500, 1, 0.2)
 
 df$x2<-sample(1:10, 2500, T)
 df$x2.miss <- df$x2
 df$x2.miss[df$group < 5 & df$x1 < 4] <- NA
 df$e<-rnorm(2500, 0, 2)
 df$y<-2 - df$x1 + 0.3*df$x2 + 0.5*df$z + df$u + df$e
                  
 # Apply the interplot to different regressions
 library(Interplot)
 
 ## 1. OLS
 m1<-lm(y~x1+x2+d+z+x1:z, data = df)
 interplot(m1, "x1", "z")
 
 ## 2. Logit with two interaction terms (the second term is of interest)
 m2<-glm(d~y+x1+x2+z+x1:z+y:z, family=binomial(link="logit"), data = df)
 interplot(m2, "y", "z")
 
 ## 3. Multilevel
 m3<-lmer(y~x1+x2+d+z+x1:z+(1|group), data = df)
 interplot(m3, "x1","z")
 
 ## 4. Multiple Imputed Data
 library(Amelia)
 
 m.imp <- amelia(df, idvars = c("y", "x2")) 
 missmap(m.imp)
 
 m4 <- lapply(m.imp$imputations, function(i) lm(y ~ x1 + x2.miss + d + x2.miss*z, data = i))
 interplot(m4, "x2.miss","z")
