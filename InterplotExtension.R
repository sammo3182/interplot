#OLS, GLS####
interplot <- function(m, var1, var2, seed=324, sims=1000, steps=100, plot=TRUE) {
  require(arm)
  require(ggplot2)
  set.seed(seed)
  m.sims <- sim(m, sims)
  
  var12 <- paste0(var2,":",var1)
  if (!var12 %in% names(m$coef)) var12 <- paste0(var1,":",var2)
  if (!var12 %in% names(m$coef)) stop(paste("Model does not include the interaction of",var1 ,"and",var2))
  coef <- data.frame(fake = seq(min(m$model[var2], na.rm=T), max(m$model[var2], na.rm=T), length.out=steps), coef1 = NA, ub = NA, lb = NA)
  
  for(i in 1:steps) {   
    coef$coef1[i] <- mean(m.sims@coef[,match(var1, names(m$coef))] + 
                            coef$fake[i]*m.sims@coef[,match(var12, names(m$coef))])
    coef$ub[i] <- quantile(m.sims@coef[,match(var1, names(m$coef))] + 
                             coef$fake[i]*m.sims@coef[,match(var12, names(m$coef))], .975)
    coef$lb[i] <- quantile(m.sims@coef[,match(var1, names(m$coef))] + 
                             coef$fake[i]*m.sims@coef[,match(var12, names(m$coef))], .025)    
  }
  
  if(plot==TRUE) {
    if(steps>5) {
      coef.plot <- ggplot(coef, aes(x = fake, y = coef1)) +                       
        geom_line() + geom_ribbon(aes(ymin=lb, ymax=ub), alpha=.5) 
    } else {
      coef.plot <- ggplot(coef, aes(x = fake, y = coef1)) +                       
        geom_point() + geom_errorbar(aes(ymin=lb, ymax=ub), width=0) + 
        scale_x_continuous(breaks = 0:steps)
    }
    return(coef.plot)
  } else {
    names(coef) <- c(var2, "coef", "ub", "lb")
    return(coef)
  }
}

#Multilevel####
MLinterplot <- function(m, var1, var2, seed=324, sims=1000, steps=100, plot=TRUE) {
  require(arm)
  require(ggplot2)
  set.seed(seed)
  m.sims <- arm::sim(m, sims) #arm::sim is a function to use the "sim" function specifically from the package "aim" rather than another package.
  var12 <- paste0(var2,":",var1)
  if (!var12 %in% unlist(dimnames(m@pp$X)[2])) var12 <- paste0(var1,":",var2)
  if (!var12 %in% unlist(dimnames(m@pp$X)[2])) stop(paste("Model does not include the interaction of",var1 ,"and",var2))
  coef <- data.frame(fake = seq(min(m@frame[var2], na.rm=T), max(m@frame[var2], na.rm=T), length.out=steps), coef1 = NA, ub = NA, lb = NA)
  
  for(i in 1:steps) {   
    coef$coef1[i] <- mean(m.sims@fixef[,match(var1, unlist(dimnames(m@pp$X)[2]))] + 
                            coef$fake[i]*m.sims@fixef[,match(var12, unlist(dimnames(m@pp$X)[2]))])
    coef$ub[i] <- quantile(m.sims@fixef[,match(var1, unlist(dimnames(m@pp$X)[2]))] + 
                             coef$fake[i]*m.sims@fixef[,match(var12, unlist(dimnames(m@pp$X)[2]))], .975)
    coef$lb[i] <- quantile(m.sims@fixef[,match(var1, unlist(dimnames(m@pp$X)[2]))] + 
                             coef$fake[i]*m.sims@fixef[,match(var12, unlist(dimnames(m@pp$X)[2]))], .025)    
  }
  
  if(plot==TRUE) {
    if(steps>5) {
      coef.plot <- ggplot(coef, aes(x = fake, y = coef1)) +                       
        geom_line() + geom_ribbon(aes(ymin=lb, ymax=ub), alpha=.5) 
    } else {
      coef.plot <- ggplot(coef, aes(x = fake, y = coef1)) +                       
        geom_point() + geom_errorbar(aes(ymin=lb, ymax=ub), width=0) + 
        scale_x_continuous(breaks = 0:steps)
    }
    return(coef.plot)
  } else {
    names(coef) <- c(var2, "coef", "ub", "lb")
    return(coef)
  }
}

#Combine####


#Example data
load("E:/Dropbox/Method/R/Homework/HW Report/nes1948-2000.Rdata")
library(foreign)
data2<-read.dta("E:/Dropbox/Method/R/Homework/HW Report/count_data.dta",
                convert.dates = TRUE, 
                convert.factors = TRUE,missing.type = TRUE,
                convert.underscore = TRUE)
load("E:/Dropbox/Instruction/Research paper/Language/cgss2010II.rdata")


Age=data$vcf0102

Gender=2-data$vcf0104

Education=data$vcf0140

Ideology=ifelse(data$vcf0803==9, NA, data$vcf0803)

Partyid=ifelse(data$vcf0303==9, NA, data$vcf0303)


#OLS
m1<-lm(Education~Age+Ideology+Partyid+Ideology:Partyid, data=data)

#Logit
m2<-glm(Gender~Education+Ideology+Partyid+Ideology:Partyid, family=binomial(link="logit"),data=data)

#Ordered Logit
require(MASS)
m3<-polr(factor(Ideology)~Education+Partyid+Education:Partyid, data=data)

#Possion
m4<-glm(art ~ fem + mar + kid5 + phd + ment+phd:ment,poisson, data=data2)

#multilevel
require(lme4)
m5<-lmer(education.1~tv+han.comp+tv:han.comp+(1|ethnicity),data=cgss2010, weights=weight)

class.r<-list(m1,m2,m3,m4,m5)
sapply(class.r, class)


#GLS(Logit，Possion， Ologit) 可以， Negative binomial(glm.nb)不行，Zelig都不行。
interplot <- function(m, var1, var2, ylab=NULL, xlab=NULL,
                      seed=324, sims=1000, steps=100, multilevel=F,plot=TRUE) {
  require(arm)
  require(ggplot2)
  set.seed(seed)
  m.sims <- arm::sim(m, sims)
  var12 <- paste0(var2,":",var1)
  if(multilevel==F){
  if (!var12 %in% names(m$coef)) var12 <- paste0(var1,":",var2)
  if (!var12 %in% names(m$coef)) stop(paste("Model does not include the interaction of",var1 ,"and",var2))
  coef <- data.frame(fake = seq(min(m$model[var2], na.rm=T), max(m$model[var2], na.rm=T), length.out=steps), coef1 = NA, ub = NA, lb = NA)
  
  for(i in 1:steps) {   
    coef$coef1[i] <- mean(m.sims@coef[,match(var1, names(m$coef))] + 
                            coef$fake[i]*m.sims@coef[,match(var12, names(m$coef))])
    coef$ub[i] <- quantile(m.sims@coef[,match(var1, names(m$coef))] + 
                             coef$fake[i]*m.sims@coef[,match(var12, names(m$coef))], .975)
    coef$lb[i] <- quantile(m.sims@coef[,match(var1, names(m$coef))] + 
                             coef$fake[i]*m.sims@coef[,match(var12, names(m$coef))], .025)    
  }
 }else{
   if (!var12 %in% unlist(dimnames(m@pp$X)[2])) var12 <- paste0(var1,":",var2)
   if (!var12 %in% unlist(dimnames(m@pp$X)[2])) stop(paste("Model does not include the interaction of",var1 ,"and",var2))
   coef <- data.frame(fake = seq(min(m@frame[var2], na.rm=T), max(m@frame[var2], na.rm=T), length.out=steps), coef1 = NA, ub = NA, lb = NA)
   
   for(i in 1:steps) {   
     coef$coef1[i] <- mean(m.sims@fixef[,match(var1, unlist(dimnames(m@pp$X)[2]))] + 
                             coef$fake[i]*m.sims@fixef[,match(var12, unlist(dimnames(m@pp$X)[2]))])
     coef$ub[i] <- quantile(m.sims@fixef[,match(var1, unlist(dimnames(m@pp$X)[2]))] + 
                              coef$fake[i]*m.sims@fixef[,match(var12, unlist(dimnames(m@pp$X)[2]))], .975)
     coef$lb[i] <- quantile(m.sims@fixef[,match(var1, unlist(dimnames(m@pp$X)[2]))] + 
                              coef$fake[i]*m.sims@fixef[,match(var12, unlist(dimnames(m@pp$X)[2]))], .025)    
   }   
 }
 if(plot==TRUE) {
   if(steps>5) {
     coef.plot <- ggplot(coef, aes(x = fake, y = coef1)) +                       
       geom_line() + geom_ribbon(aes(ymin=lb, ymax=ub), alpha=.5) + theme_bw()+
       ylab(ylab) + xlab(xlab)
   } else {
     coef.plot <- ggplot(coef, aes(x = fake, y = coef1)) +                       
       geom_point() + geom_errorbar(aes(ymin=lb, ymax=ub), width=0) + 
       scale_x_continuous(breaks = 0:steps) + theme_bw()+
       ylab(ylab) + xlab(xlab)
   }
   return(coef.plot)
 } else {
   names(coef) <- c(var2, "coef", "ub", "lb")
   return(coef)
 }
}
  



interplot <- function(m, var1, var2, ylab=NULL, xlab=NULL,
                      seed=324, sims=1000, steps=100, plot=TRUE) {
  require(arm)
  require(ggplot2)
  set.seed(seed)
  m.sims <- arm::sim(m, sims)
  var12 <- paste0(var2,":",var1)
  if(class(m)!="lmerMod"){
    if (!var12 %in% names(m$coef)) var12 <- paste0(var1,":",var2)
    if (!var12 %in% names(m$coef)) stop(paste("Model does not include the interaction of",var1 ,"and",var2))
    coef <- data.frame(fake = seq(min(m$model[var2], na.rm=T), max(m$model[var2], na.rm=T), length.out=steps), coef1 = NA, ub = NA, lb = NA)
    
    for(i in 1:steps) {   
      coef$coef1[i] <- mean(m.sims@coef[,match(var1, names(m$coef))] + 
                              coef$fake[i]*m.sims@coef[,match(var12, names(m$coef))])
      coef$ub[i] <- quantile(m.sims@coef[,match(var1, names(m$coef))] + 
                               coef$fake[i]*m.sims@coef[,match(var12, names(m$coef))], .975)
      coef$lb[i] <- quantile(m.sims@coef[,match(var1, names(m$coef))] + 
                               coef$fake[i]*m.sims@coef[,match(var12, names(m$coef))], .025)    
    }
  }else{
    if (!var12 %in% unlist(dimnames(m@pp$X)[2])) var12 <- paste0(var1,":",var2)
    if (!var12 %in% unlist(dimnames(m@pp$X)[2])) stop(paste("Model does not include the interaction of",var1 ,"and",var2))
    coef <- data.frame(fake = seq(min(m@frame[var2], na.rm=T), max(m@frame[var2], na.rm=T), length.out=steps), coef1 = NA, ub = NA, lb = NA)
    
    for(i in 1:steps) {   
      coef$coef1[i] <- mean(m.sims@fixef[,match(var1, unlist(dimnames(m@pp$X)[2]))] + 
                              coef$fake[i]*m.sims@fixef[,match(var12, unlist(dimnames(m@pp$X)[2]))])
      coef$ub[i] <- quantile(m.sims@fixef[,match(var1, unlist(dimnames(m@pp$X)[2]))] + 
                               coef$fake[i]*m.sims@fixef[,match(var12, unlist(dimnames(m@pp$X)[2]))], .975)
      coef$lb[i] <- quantile(m.sims@fixef[,match(var1, unlist(dimnames(m@pp$X)[2]))] + 
                               coef$fake[i]*m.sims@fixef[,match(var12, unlist(dimnames(m@pp$X)[2]))], .025)    
    }   
  }
  if(plot==TRUE) {
    if(steps>5) {
      coef.plot <- ggplot(coef, aes(x = fake, y = coef1)) +                       
        geom_line() + geom_ribbon(aes(ymin=lb, ymax=ub), alpha=.5) + theme_bw()+
        ylab(ylab) + xlab(xlab)
    } else {
      coef.plot <- ggplot(coef, aes(x = fake, y = coef1)) +                       
        geom_point() + geom_errorbar(aes(ymin=lb, ymax=ub), width=0) + 
        scale_x_continuous(breaks = 0:steps) + theme_bw()+
        ylab(ylab) + xlab(xlab)
    }
    return(coef.plot)
  } else {
    names(coef) <- c(var2, "coef", "ub", "lb")
    return(coef)
  }
}


#Test
interplot(m=m1, var1="Ideology", var2="Partyid")
interplot(m=m2, var1="Ideology", var2="Partyid")
interplot(m=m3, var1="Education", var2="Partyid")
interplot(m=m4, var1="phd", var2="ment")
interplot(m=m5, var1="tv", var2="han.comp")

