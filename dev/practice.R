devtools::source_gist(9112634)
package(c("ggplot2", "interplot"))

m_cyl <- lmer(mpg ~ wt * hp + (wt | cyl), mtcars)
m_cyl <- lm(mpg ~ wt * cyl, mtcars)


m <- m_cyl
var1 <- "hp"
var2 <- "wt"
plot = TRUE
point = FALSE
sims = 5000
xmin = NA
xmax = NA
ercolor = NA
esize = .5
ralpha = .5
rfill = "grey70"
var2_dt <- NA


hist <- TRUE

var2_dt <- m@frame$hp

yrange<- c(coef$ub, coef$lb, var2_dt)

maxdiff<-(max(yrange)-min(yrange))

hist.out<-hist(var2_dt,breaks=80,plot=FALSE)
n.hist<-length(hist.out$mids)
dist<-hist.out$mids[2]-hist.out$mids[1]
hist.max<-max(hist.out$counts)

histX<-data.frame(ymin=rep(min(yrange)-maxdiff/5,n.hist),
                  ymax=hist.out$counts/hist.max*maxdiff/5+min(yrange)-maxdiff/5,
                  xmin=hist.out$mids-dist/2,
                  xmax=hist.out$mids+dist/2)



#interplot.plot(m = coef, var1 = "cyl", var2 = "wt") 

p1 <- ggplot()
p1 <- p1 + geom_rect(data=histX,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
                     colour="gray50",alpha=0,size=0.5)


p1 + geom_line(data = m, aes_string(x = "fake", y = "coef1")) + 
  geom_ribbon(data = m, aes_string(x = "fake",  ymin = "lb", ymax = "ub"), alpha = ralpha, color = ercolor, fill = rfill) + 
  ylab(NULL) + xlab(NULL)

######################################

# adjustCI option:

m_cyl <- lm(mpg ~ wt * cyl, mtcars)

interplot(m_cyl, var1 = "wt", var2 = "cyl")

mtcars$cyl_f <- as.factor(mtcars$cyl)
m_cylF <- lm(mpg ~ wt * cyl_f, mtcars)
interplot.default(m_cylF, var2 = "wt", var1 = "cyl_f", adjCI = TRUE)



pacman::p_load(ggplot2, interactionTest)

m <- m_cyl
var1 <- "cyl"
var2 <- "wt"
plot = TRUE
ci = .95
hist = FALSE
var2_dt = NA
point = FALSE
sims = 5000
steps = NULL
xmin = NA
xmax = NA
ercolor = NA
esize = .5
ralpha = .5
rfill = "grey70"

  
m.class <- class(m)
m.sims <- arm::sim(m, sims)


### For factor base terms###
factor_v1 <- factor_v2 <- FALSE

if (is.factor(eval(parse(text = paste0("m$model$", var1)))) & is.factor(eval(parse(text = paste0("m$model$", 
                                                                                                 var2))))) 
  stop("The function does not support interactions between two factors.")


if (is.factor(eval(parse(text = paste0("m$model$", var1))))) {
  var1_bk <- var1
  var1 <- paste0(var1, eval(parse(text = paste0("m$xlevel$", var1))))
  factor_v1 <- TRUE
  ifelse(var1 == var2, var12 <- paste0("I(", var1, "^2)"), var12 <- paste0(var2, 
                                                                           ":", var1)[-1])
  
  # the first category is censored to avoid multicolinarity
  for (i in seq(var12)) {
    if (!var12[i] %in% names(m$coef)) 
      var12[i] <- paste0(var1, ":", var2)[-1][i]
    if (!var12[i] %in% names(m$coef)) 
      stop(paste("Model does not include the interaction of", 
                 var1, "and", var2, "."))
  }
  
} else if (is.factor(eval(parse(text = paste0("m$model$", var2))))) {
  var2_bk <- var2
  var2 <- paste0(var2, eval(parse(text = paste0("m$xlevel$", var2))))
  factor_v2 <- TRUE
  ifelse(var1 == var2, var12 <- paste0("I(", var1, "^2)"), var12 <- paste0(var2, 
                                                                           ":", var1)[-1])
  
  # the first category is censored to avoid multicolinarity
  for (i in seq(var12)) {
    if (!var12[i] %in% names(m$coef)) 
      var12[i] <- paste0(var1, ":", var2)[-1][i]
    if (!var12[i] %in% names(m$coef)) 
      stop(paste("Model does not include the interaction of", 
                 var1, "and", var2, "."))
  }
  
} else {
  ifelse(var1 == var2, var12 <- paste0("I(", var1, "^2)"), var12 <- paste0(var2, 
                                                                           ":", var1))
  
  # the first category is censored to avoid multicolinarity
  for (i in seq(var12)) {
    if (!var12[i] %in% names(m$coef)) 
      var12[i] <- paste0(var1, ":", var2)[i]
    if (!var12[i] %in% names(m$coef)) 
      stop(paste("Model does not include the interaction of", 
                 var1, "and", var2, "."))
  }
}

################### 

if (factor_v2) {
  xmin <- 0
  xmax <- 1
  steps <- 2
} else {
  if (is.na(xmin)) 
    xmin <- min(m$model[var2], na.rm = T)
  if (is.na(xmax)) 
    xmax <- max(m$model[var2], na.rm = T)
  
  if (is.null(steps)) {
    steps <- eval(parse(text = paste0("length(unique(na.omit(m$model$", 
                                      var2, ")))")))
  }
  
  
  if (steps > 100) 
    steps <- 100  # avoid redundant calculation
}

coef <- data.frame(fake = seq(xmin, xmax, length.out = steps), coef1 = NA, 
                   ub = NA, lb = NA)
coef_df <- data.frame(fake = numeric(0), coef1 = numeric(0), ub = numeric(0), 
                      lb = numeric(0), model = character(0))

if (factor_v1) {
  for (j in 1:(length(eval(parse(text = paste0("m$xlevel$", var1_bk)))) - 
               1)) {
    # only n - 1 interactions; one category is avoided against
    # multicolinarity
    
    for (i in 1:steps) {
      coef$coef1[i] <- mean(m.sims@coef[, match(var1[j + 1], 
                                                names(m$coef))] + coef$fake[i] * m.sims@coef[, match(var12[j], 
                                                                                                     names(m$coef))])
      coef$ub[i] <- quantile(m.sims@coef[, match(var1[j + 1], 
                                                 names(m$coef))] + coef$fake[i] * m.sims@coef[, match(var12[j], 
                                                                                                      names(m$coef))], 1 - (1 - ci) / 2)
      coef$lb[i] <- quantile(m.sims@coef[, match(var1[j + 1], 
                                                 names(m$coef))] + coef$fake[i] * m.sims@coef[, match(var12[j], 
                                                                                                      names(m$coef))], (1 - ci) / 2)
    }
    
    if (plot == TRUE) {
      coef$value <- var1[j + 1]
      coef_df <- rbind(coef_df, coef)
      if (hist == TRUE) {
        if (is.na(var2_dt)) {
          var2_dt <- eval(parse(text = paste0("m$model$", var2)))
        } else {
          var2_dt <- var2_dt
        }
      }
      coef_df$value <- as.factor(coef_df$value)
      interplot.plot(m = coef_df, hist = hist, var2_dt = var2_dt, steps = steps, 
                     point = point, ercolor = ercolor, esize = esize, ralpha = ralpha, 
                     rfill = rfill, ...) + facet_grid(. ~ value)
    } else {
      names(coef) <- c(var2, "coef", "ub", "lb")
      return(coef)
    }
  }
} else if (factor_v2) {
  for (j in 1:(length(eval(parse(text = paste0("m$xlevel$", var2_bk)))) - 
               1)) {
    # only n - 1 interactions; one category is avoided against
    # multicolinarity
    
    for (i in 1:steps) {
      coef$coef1[i] <- mean(m.sims@coef[, match(var1, names(m$coef))] + 
                              coef$fake[i] * m.sims@coef[, match(var12[j], names(m$coef))])
      coef$ub[i] <- quantile(m.sims@coef[, match(var1, names(m$coef))] + 
                               coef$fake[i] * m.sims@coef[, match(var12[j], names(m$coef))], 
                             1 - (1 - ci) / 2)
      coef$lb[i] <- quantile(m.sims@coef[, match(var1, names(m$coef))] + 
                               coef$fake[i] * m.sims@coef[, match(var12[j], names(m$coef))], 
                             (1 - ci) / 2)
    }
    
    if (plot == TRUE) {
      coef$value <- var2[j + 1]
      coef_df <- rbind(coef_df, coef)
      if (hist == TRUE) {
        if (is.na(var2_dt)) {
          var2_dt <- eval(parse(text = paste0("m$model$", var2)))
        } else {
          var2_dt <- var2_dt
        }
      }
      coef_df$value <- as.factor(coef_df$value)
      interplot.plot(m = coef_df, hist = hist, steps = steps, var2_dt = var2_dt, 
                     point = point, ercolor = ercolor, esize = esize, ralpha = ralpha, 
                     rfill = rfill, ...) + facet_grid(. ~ value)
    } else {
      names(coef) <- c(var2, "coef", "ub", "lb")
      return(coef)
    }
  }
  
  
  
} else {
  ## Correct marginal effect for quadratic terms
  multiplier <- if (var1 == var2) 
    2 else 1
  
  for (i in 1:steps) {
    coef$coef1[i] <- mean(m.sims@coef[, match(var1, names(m$coef))] + 
                            multiplier * coef$fake[i] * m.sims@coef[, match(var12, 
                                                                            names(m$coef))])
    coef$ub[i] <- quantile(m.sims@coef[, match(var1, names(m$coef))] + 
                             multiplier * coef$fake[i] * m.sims@coef[, match(var12, 
                                                                             names(m$coef))], 1 - (1 - ci) / 2)
    coef$lb[i] <- quantile(m.sims@coef[, match(var1, names(m$coef))] + 
                             multiplier * coef$fake[i] * m.sims@coef[, match(var12, 
                                                                             names(m$coef))], (1 - ci) / 2)
  }
  
## FDR correction
# ub = mean + qnorm(.975) * sd
  coef$sd <- (coef$ub - coef$coef1) / qnorm(.975) 
  tAdj <- fdrInteraction(coef$coef1, coef$sd, df = m$df, level = .95) # calculate critical t
  coef$ub <- coef$coef1 + tAdj * coef$sd
  coef$lb <- coef$coef1 - tAdj * coef$sd
  
  if (plot == TRUE) {
    if (hist == TRUE) {
      if (is.na(var2_dt)) {
        var2_dt <- eval(parse(text = paste0("m$model$", var2)))
      } else {
        var2_dt <- var2_dt
      }
    }
    interplot.plot(m = coef, steps = steps, hist = hist, var2_dt = var2_dt, 
                   point = point, ercolor = ercolor, esize = esize, ralpha = ralpha, 
                   rfill = rfill, ...)
  } else {
    names(coef) <- c(var2, "coef", "ub", "lb")
    return(coef)
  }
  
}

