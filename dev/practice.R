pacman::p_load(ggplot2, lme4, dplyr, purrr)

pew1.w <- read.csv("dev/pew1_w.csv")

m <- glm(formula=meritocracy~ginicnty+income_i+ginicnty:income_i+income_cnty+black_cnty+
           perc_bush04+pop_cnty+educ_i+age_i+gender_i+unemp_i+union_i+partyid_i+
           ideo_i+attend_i+survid2006+survid2007+survid2009,
         data=pew1.w,family=binomial(link="logit"))

var1 = "ginicnty"
var2 = "income_i"
sims = 100


plot = TRUE
steps = eval(parse(text = paste0("length(unique(na.omit(m$model$", var2, ")))")))
ci = .95
adjCI = FALSE
hist = FALSE
predictPro = TRUE
var2_vals = c(min(pew1.w$income_i), max(pew1.w$income_i))
var2_dt = NA
point = FALSE

xmin = NA
xmax = NA
ercolor = NA
esize = 0.5
ralpha = 0.5
rfill = "grey70"


load("dev/cgss2010.rdata")

m <-  MASS::polr(trustLocal ~ latentPol + latentSoc + mandarin + I(mandarin^2) + female + age + minority + edu + incomeFam + party + hukouRural + newMedia + prov, weights = wt, Hess = FALSE, data = cgss2010)

interplot(m, var1 = "mandarin", var2 = "mandarin", predPro = TRUE, var2_vals = c(min(cgss2010$mandarin, na.rm = TRUE), max(cgss2010$mandarin, na.rm = TRUE)))



# m <- lmer(mpg ~ wt * hp + (1 | cyl), data = mtcars)
var1 = "mandarin"
var2 = "mandarin"
var2_vals = c(min(cgss2010$mandarin, na.rm = TRUE), max(cgss2010$mandarin, na.rm = TRUE))

sims = 100


plot = TRUE
steps = eval(parse(text = paste0("length(unique(na.omit(m$model$", var2, ")))")))
ci = .95
adjCI = FALSE
hist = FALSE
predPro = TRUE
var2_dt = NA
point = FALSE

xmin = NA
xmax = NA
ercolor = NA
esize = 0.5
ralpha = 0.5
rfill = "grey70"


m.class <- class(m)
m.sims <- arm::sim(m, sims)


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



  if (is.na(xmin)) 
    xmin <- min(m$model[var2], na.rm = T)
  if (is.na(xmax)) 
    xmax <- max(m$model[var2], na.rm = T)
  
  if (is.null(steps)) {
    steps <- eval(parse(text = paste0("length(unique(na.omit(m$model$", 
                                      var2, ")))")))
  }
  

coef <- data.frame(fake = seq(xmin, xmax, length.out = steps), coef1 = NA, 
                   ub = NA, lb = NA)
coef_df <- data.frame(fake = numeric(0), coef1 = numeric(0), ub = numeric(0), 
                      lb = numeric(0), model = character(0))


    if(is.null(var2_vals)) stop("The predicted probabilities cannot be estimated without defining 'var2_vals'.")
    
    df <- data.frame(m$model)
    df[[names(m@flist)]] <- NULL # omit L2 var
    if(sum(grep("X.weights.", names(df))) != 0) df <- select(df, -X.weights.) # removed the weights
    df_temp <- select(df, 1) # save the dependent variable separately
    df <- df[-1] %>% # get ride of the dv in case it's a factor
      map(function(var){
        if(is.factor(var)){
          model.matrix(~ var - 1)[, -1] %>% 
            # get rid of the first (reference) group
          as.data.frame()
      }else{
          as.numeric(var) # in case the initial one is a "labelled" class
        }
      })
      
    
    for(i in seq(df)){ 
      # use for loop to avoid the difficulty of flatting list containing vectors and matrices
      if(!is.data.frame(df[[i]])){
        # keep track the var names
        namesUpdate <- c(names(df_temp), names(df)[[i]])
        df_temp <- cbind(df_temp, df[[i]])
        names(df_temp) <- namesUpdate
      }else{
        df_temp <- cbind(df_temp, df[[i]])
      }
    }
    
    df <- df_temp
    
    names(df)[1] <- "(Intercept)" # replace DV with intercept
    df$`(Intercept)` <- 1
    
    if(var1 == var2){ # correct the name of squares
      names(df) <- sub("I\\.(.*)\\.2\\.", "I\\(\\1\\^2\\)", names(df))
    }
    
    iv_medians <- summarize_all(df, funs(median(., na.rm = TRUE))) 
    
    fake_data <- iv_medians[rep(1:nrow(iv_medians), each=steps*length(var2_vals)), ] 
    fake_data[[var1]] <- with(df, rep(seq(min(get(var1)), max(get(var1)), length.out=steps),
                                      steps=length(var2_vals)))
    fake_data[[var2]] <- rep(var2_vals, each=steps)
    fake_data[[var12]] <- fake_data[[var1]] * fake_data[[var2]]
    
    pp <- rowMeans(plogis(data.matrix(fake_data) %*% t(data.matrix(m.sims@coef))))
    row_quantiles <- function (x, probs) {
      naValue <- NA
      storage.mode(naValue) <- storage.mode(x)
      nrow <- nrow(x)
      q <- matrix(naValue, nrow = nrow, ncol = length(probs))
      if (nrow > 0L) {
        t <- quantile(x[1L, ], probs = probs)
        colnames(q) <- names(t)
        q[1L, ] <- t
        if (nrow >= 2L) {
          for (rr in 2:nrow) {
            q[rr, ] <- quantile(x[rr, ], probs = probs)
          }
        }
      }
      else {
        t <- quantile(0, probs = probs)
        colnames(q) <- names(t)
      }
      q <- drop(q)
      q
    }
    pp_bounds <- row_quantiles(plogis(data.matrix(fake_data) %*% t(data.matrix(m.sims@coef))), prob = c((1 - ci)/2, 1 - (1 - ci)/2))
    pp <- cbind(pp, pp_bounds)
    pp <- pp*100
    colnames(pp) <- c("coef1", "lb", "ub")
    pp <- cbind(fake_data[, c(var1, var2)], pp)
    
    
    pp[,var2] <- as.factor(pp[,var2])
    
    names(pp)[1] <- "fake"
    names(pp)[2] <- "value"
    
    coef <- pp
  
  
  if(adjCI == TRUE){
    ## FDR correction
    coef$sd <- (coef$ub - coef$coef1) / qnorm(1 - (1 - ci)/2) 
    tAdj <- fdrInteraction(coef$coef1, coef$sd, df = m$df, level = .95) # calculate critical t
    coef$ub <- coef$coef1 + tAdj * coef$sd
    coef$lb <- coef$coef1 - tAdj * coef$sd
  }
  
  
  if (plot == TRUE) {
    if (hist == TRUE) {
      if (is.na(var2_dt)) {
        var2_dt <- eval(parse(text = paste0("m$model$", var2)))
      } else {
        var2_dt <- var2_dt
      }
    }
    interplot.plot(m = coef, steps = steps, hist = hist, var2_dt = var2_dt, , predPro = predPro, var2_vals = var2_vals, point = point, ercolor = ercolor, esize = esize, ralpha = ralpha, rfill = rfill, ...)
  } else {
    if(predPro == TRUE){
      names(coef) <- c(var2, paste0("values_in_", var1), "coef", "ub", "lb")
    } else {
      names(coef) <- c(var2, "coef", "ub", "lb")
    }
    
    return(coef)
  }
}




pred_probs <- function(m, var1, var2, var2_vals,
                       steps = 20, sims = 1000) {
  m.sims <- arm::sim(m, sims)

  # Find name of interaction term
  ifelse(var1 == var2, var12 <- paste0("I(", var1, "^2)"), var12 <- paste0(var2,":", var1))
  for (i in seq(var12)) {
    if (!var12[i] %in% names(m$coef)) 
      var12[i] <- paste0(var1, ":", var2)[i]
    if (!var12[i] %in% names(m$coef)) 
      stop(paste("Model does not include the interaction of", 
                 var1, "and", var2, "."))
  }
  df <- data.frame(m$model)
  names(df)[1] <- "(Intercept)" # replace DV with intercept
  df$`(Intercept)` <- 1
  
  iv_medians <- summarize_all(df, funs(median(., na.rm = TRUE))) 
  
  fake_data <- iv_medians[rep(1:nrow(iv_medians), each=steps*length(var2_vals)), ] 
  fake_data[[var1]] <- with(df, rep(seq(min(get(var1)), max(get(var1)), length.out=steps),
                                    steps=length(var2_vals)))
  fake_data[[var2]] <- rep(var2_vals, each=steps)
  fake_data[[var12]] <- fake_data[[var1]] * fake_data[[var2]]
  
  pp <- rowMeans(plogis(data.matrix(fake_data) %*% t(data.matrix(m.sims@coef))))
  row_quantiles <- function (x, probs) {
    naValue <- NA
    storage.mode(naValue) <- storage.mode(x)
    nrow <- nrow(x)
    q <- matrix(naValue, nrow = nrow, ncol = length(probs))
    if (nrow > 0L) {
      t <- quantile(x[1L, ], probs = probs)
      colnames(q) <- names(t)
      q[1L, ] <- t
      if (nrow >= 2L) {
        for (rr in 2:nrow) {
          q[rr, ] <- quantile(x[rr, ], probs = probs)
        }
      }
    }
    else {
      t <- quantile(0, probs = probs)
      colnames(q) <- names(t)
    }
    q <- drop(q)
    q
  }
  pp_bounds <- row_quantiles(plogis(data.matrix(fake_data) %*% t(data.matrix(m.sims@coef))), prob = c((1 - ci)/2, 1 - (1 - ci)/2))
  pp <- cbind(pp, pp_bounds)
  pp <- pp*100
  colnames(pp) <- c("coef1", "lb", "ub")
  pp <- cbind(fake_data[, c(var1, var2)], pp)
  
  
  pp[,var2] <- as.factor(pp[,var2])
  
  names(pp)[1] <- "fake"
  names(pp)[2] <- "value"
 
  # return(pp)
   
  # coef.plot <- ggplot()
  # coef.plot + geom_line(data = m, aes_string(x = "fake", y = "coef1"), color = "black") + 
  #   geom_ribbon(data = m, aes_string(x = "fake", ymin = "lb", ymax = "ub", fill = "value"), alpha = .5) + ylab(NULL) + xlab(NULL)
  
}


interplot.plot(m = pp, steps = steps, hist = hist, predPro = TRUE, var2_dt = pew1.w$ginicnty, sims = 1000)

interplot(m, var1 = "ginicnty",var2 = "income_i", predPro = TRUE, var2_vals = c(min(pew1.w$income_i), median(pew1.w$income_i), max(pew1.w$income_i)))

interplot.plot(m = coef, steps = steps, hist = hist, predictPro = predictPro, var2_vals = var2_vals, var2_dt = var2_dt, point = point, ercolor = ercolor, esize = esize, ralpha = ralpha, rfill = rfill)


m <- lm(mpg ~ wt * cyl, data = mtcars)
var2 <- "wt"
var1 <- "cyl"
plot = TRUE
steps = NULL
ci = .95
adjCI = FALSE
hist = FALSE
var2_dt = NA
predPro = FALSE
var2_vals = NULL
point = FALSE
sims = 1000
xmin = NA
xmax = NA
ercolor = NA
esize = 0.5
ralpha = 0.5
rfill = "grey70"
stats_cp = "none"
txt_caption = NULL
facet_labs = NULL

interplot(m = m, var1 = var1, var2 = var2, plot = FALSE)

library(mitools)

data(smi)
with(smi, table(sex, drkfre))
model1 <- with(smi, glm(drinkreg ~ wave * sex, family = binomial()))

interplot(model1, var1 = "sex", var2 = "wave")

MIcombine(model1)
summary(MIcombine(model1))
