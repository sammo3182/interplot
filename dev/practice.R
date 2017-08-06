pacman::p_load(ggplot2, lme4, dplyr)

pew1.w <- read.csv("dev/pew1_w.csv")

m <- glm(formula=meritocracy~ginicnty+income_i+ginicnty:income_i+income_cnty+black_cnty+
                perc_bush04+pop_cnty+educ_i+age_i+gender_i+unemp_i+union_i+partyid_i+
                ideo_i+attend_i+survid2006+survid2007+survid2009,
              data=pew1.w,family=binomial(link="logit"))

# m <- lmer(mpg ~ wt * hp + (1 | cyl), data = mtcars)
var1 = "ginicnty"
var2 = "income_i"
var2_vals = c(min(pew1.w$income_i), max(pew1.w$income_i))

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
