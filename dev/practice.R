pacman::p_load(ggplot2, lme4, dplyr)
m <- lmer(mpg ~ wt * hp + (1 | cyl), data = mtcars)
var1 = "wt"
var2 = "hp"
var2_vals = c(min(mtcars$hp), max(mtcars$hp))
times = 20
sims = 1000

pred_probs(m, var1 = "wt", var2 = "hp", var2_vals = c(min(mtcars$hp), max(mtcars$hp)))

pred_probs <- function(m, var1, var2, var2_vals,
                       times = 20, sims = 1000) {
  m_sims <- arm::sim(m, sims)
  df <- data.frame(m@frame)
  df[[names(m@flist)]] <- NULL # omit L2 var
  names(df)[1] <- "(Intercept)" # replace DV with intercept
  df$`(Intercept)` <- 1
  
  # Find name of interaction term
  ifelse(var1 == var2, var12 <- paste0("I(", var1, "^2)"), 
         var12 <- paste0(var2, ":", var1))
  if (!var12 %in% unlist(dimnames(m@pp$X)[2])) 
    var12 <- paste0(var1, ":", var2)
  if (!var12 %in% unlist(dimnames(m@pp$X)[2])) 
    stop(paste("Model does not include the interaction of", var1, "and", 
               var2, "."))
  
  iv_medians <- summarize_all(df, funs(median(., na.rm = TRUE))) 
  
  fake_data <- iv_medians[rep(1:nrow(iv_medians), each=times*length(var2_vals)), ] 
  fake_data[[var1]] <- with(df, rep(seq(min(get(var1)), max(get(var1)), length.out=times),
                                    times=length(var2_vals)))
  fake_data[[var2]] <- rep(var2_vals, each=times)
  fake_data[[var12]] <- fake_data[[var1]] * fake_data[[var2]]
  
  pp <- rowMeans(plogis(data.matrix(fake_data) %*% t(data.matrix(m_sims@fixef))))
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
  pp_bounds <- row_quantiles(plogis(data.matrix(fake_data) %*% t(data.matrix(m_sims@fixef))), prob = c(.025, .975))
  pp <- cbind(pp, pp_bounds)
  pp <- pp*100
  colnames(pp) <- c("coef1", "lb", "ub")
  pp <- cbind(fake_data, pp)
  
  pp[,var2] <- as.factor(pp[,var2])
  
  m <- select(pp, c(2, 3, 5, 6, 7)) 
  names(m)[1] <- "fake"
  names(m)[2] <- "value"
 
  return(m)
   
  # coef.plot <- ggplot()
  # coef.plot + geom_line(data = m, aes_string(x = "fake", y = "coef1"), color = "black") + 
  #   geom_ribbon(data = m, aes_string(x = "fake", ymin = "lb", ymax = "ub", fill = "value"), alpha = .5) + ylab(NULL) + xlab(NULL)
  
}