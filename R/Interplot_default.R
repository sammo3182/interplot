if(getRversion() >= "2.15.1") utils::globalVariables(c(".", "X.weights.", "ks_diff", "ks.test"))

#' Plot Conditional Coefficients in (Generalized) Linear Models with Interaction Terms
#' 
#' \code{interplot.default} is a method to calculate conditional coefficient estimates from the results of (generalized) linear regression models with interaction terms. 
#' 
#' @param m A model object including an interaction term, or, alternately, a data frame recording conditional coefficients.
#' @param var1 The name (as a string) of the variable of interest in the interaction term; its conditional coefficient estimates will be plotted.
#' @param var2 The name (as a string) of the other variable in the interaction term.
#' @param plot A logical value indicating whether the output is a plot or a dataframe including the conditional coefficient estimates of var1, their upper and lower bounds, and the corresponding values of var2.
#' @param steps Desired length of the sequence. A non-negative number, which for seq and seq.int will be rounded up if fractional. The default is 100 or the unique categories in the \code{var2} (when it is less than 100. Also see \code{\link{unique}}).
#' @param ci A numeric value defining the confidence intervals. The default value is 95\% (0.95).
#' @param adjCI A logical value indication if applying the adjustment of confidence intervals to control the false discovery rate following the Esarey and Sumner (2017) procedure. (See also Benjamini and Hochberg 1995.) The default is FALSE; the plot presents the confidence intervals suggested by Brambor, Clark, and Golder (2006). 
#' @param hist A logical value indicating if there is a histogram of `var2` added at the bottom of the conditional effect plot.
#' @param var2_dt A numerical value indicating the frequency distribution of `var2`. It is only used when `hist == TRUE`. When the object is a model, the default is the distribution of `var2` of the model. 
#' @param predPro A logical value with default of `FALSE`. When the `m` is an object of class `glm` and the argument is set to `TRUE`, the function will plot predicted probabilities at the values given by `var2_vals`. 
#' @param var2_vals A numerical value indicating the values the predicted probabilities are estimated, when `predPro` is `TRUE`. 
#' @param point A logical value determining the format of plot. By default, the function produces a line plot when var2 takes on ten or more distinct values and a point (dot-and-whisker) plot otherwise; option TRUE forces a point plot.
#' @param sims Number of independent simulation draws used to calculate upper and lower bounds of coefficient estimates: lower values run faster; higher values produce smoother curves.
#' @param xmin A numerical value indicating the minimum value shown of x shown in the graph. Rarely used.
#' @param xmax A numerical value indicating the maximum value shown of x shown in the graph. Rarely used.
#' @param ercolor A character value indicating the outline color of the whisker or ribbon.
#' @param esize A numerical value indicating the size of the whisker or ribbon.
#' @param ralpha A numerical value indicating the transparency of the ribbon.
#' @param rfill A character value indicating the filling color of the ribbon.
#' @param stats_cp A character value indicating what statistics to present as the plot note. Three options are available: "none", "ci", and "ks". The default is "none". See the Details for more information.
#' @param txt_caption A character string to add a note for the plot, a value will sending to \code{ggplot2::labs(caption = txt_caption))}.
#' @param facet_labs An optional character vector of facet labels to be used when plotting an interaction with a factor variable.
#' @param ... Other ggplot aesthetics arguments for points in the dot-whisker plot or lines in the line-ribbon plots. Not currently used.
#' 
#' @details \code{interplot.default} is a S3 method from the \code{interplot}. It works on two classes of objects:
#' \itemize{
#'   \item Ordinary linear models (object class: \code{lm});
#'   \item Generalized linear models (object class: \code{glm}).
#'   }
#' 
#' Because the output function is based on \code{\link[ggplot2]{ggplot}}, any additional arguments and layers supported by \code{ggplot2} can be added with the \code{+}. 
#' 
#' \code{interplot} visualizes the conditional effect based on simulated marginal effects. The simulation provides a probabilistic distribution of moderation effect of the conditioning variable (\code{var2}) at every preset values (including the minimum and maximum values) of the conditioned variable (\code{var1}), denoted as Emin and Emax. This output allows the function to further examine the conditional effect statistically in two ways. One is to examine if the distribution of \eqn{Emax - Emin} covers zero. The other is to directly compare Emin and Emax through statistical tools for distributional comparisons. Users can choose either method by setting the argument \code{stats_cp} to "ci" or "ks".
#' \itemize{
#'   \item "ci" provides the confidence interval of the difference of \eqn{Emax - Emin}. An interval including 0 suggests no statistical difference before and after the conditional effect is applied, and vise versa.
#'   \item "ks" presents the result of a two-sample Kolmogorov-Smirnov test of the simulated distributions of Emin and Emax. The output includes a D statistics and a p-value of the null hypothesis that the two distributions come from the same distribution at the 0.05 level.
#' }
#' 
#' See an illustration in the package vignette.
#' 
#' @return The function returns a \code{ggplot} object.
#' 
#' @importFrom arm sim
#' @importFrom stats quantile
#' @importFrom stats qnorm
#' @importFrom stats median
#' @importFrom stats plogis
#' @importFrom stats model.matrix
#' @importFrom purrr map
#' @importFrom interactionTest fdrInteraction
#' @import  ggplot2
#' @import  dplyr
#' 
#' @source Benjamini, Yoav, and Yosef Hochberg. 1995. "Controlling the False
#' Discovery Rate: A Practical and Powerful Approach to Multiple Testing".
#' Journal of the Royal Statistical Society, Series B 57(1): 289--300.
#'
#' Brambor, Thomas, William Roberts Clark, and Matt Golder.
#' "Understanding interaction models: Improving empirical analyses". Political
#' Analysis 14.1 (2006): 63-82.
#'
#' Esarey, Justin, and Jane Lawrence Sumner. 2015. "Marginal Effects in
#' Interaction Models: Determining and Controlling the False Positive Rate".
#' URL: \url{http://jee3.web.rice.edu/interaction-overconfidence.pdf}.
#' 
#' @export


# S3 method for class 'lm' and 'glm'
interplot.default <- function(m,
                              var1,
                              var2,
                              plot = TRUE,
                              steps = NULL,
                              ci = .95,
                              adjCI = FALSE,
                              hist = FALSE,
                              var2_dt = NA,
                              predPro = FALSE,
                              var2_vals = NULL,
                              point = FALSE,
                              sims = 1000,
                              xmin = NA,
                              xmax = NA,
                              ercolor = NA,
                              esize = 0.5,
                              ralpha = 0.5,
                              rfill = "grey70",
                              stats_cp = "none",
                              txt_caption = NULL,
                              facet_labs = NULL,
                              ...) {
  
  m.class <- class(m)
  m.sims <- arm::sim(m, sims)
  
  
  if (predPro == TRUE & all(m.class == "lm"))
    stop("Predicted probability is estimated only for general linear models.")
  
  # Coefficient data frame ####
  ## Factorial base terms
  factor_v1 <- is.factor(eval(parse(text = paste0("m$model$", var1))))
  factor_v2 <- is.factor(eval(parse(text = paste0("m$model$", var2))))
  
  if (factor_v1 & factor_v2)
    stop("The function does not support interactions between two factors.")
  
  if ((factor_v1 | factor_v2) & predPro == TRUE)
    stop("The current version does not support estimating predicted probabilities for factor base terms.")
  
  if (factor_v1 | factor_v2) {
    ls_results <- extract_coef_fac(
      factor_v1 = factor_v1,
      factor_v2 = factor_v2,
      m = m,
      m.sims = m.sims,
      var1 = var1,
      var2 = var2, 
      plot = plot,
      steps = steps,
      ci = ci,
      adjCI = adjCI,
      predPro = predPro,
      var2_vals = var2_vals,
      point = point,
      xmin = xmin,
      xmax = xmax
    )
  } else {
    ls_results <- extract_coef_num(
      m = m,
      m.sims = m.sims,
      var1 = var1,
      var2 = var2, 
      plot = plot,
      steps = steps,
      ci = ci,
      adjCI = adjCI,
      predPro = predPro,
      var2_vals = var2_vals,
      point = point,
      xmin = xmin,
      xmax = xmax
    )
  }
  
  coef_df <- ls_results[[1]]
  ci_diff <- ls_results[[2]]
  steps <- ls_results[[3]]
  
  # Plotting ####
  
  if (hist == TRUE & all(is.na(var2_dt))) { # when var2_dt has values, is.na returns multiple values
    var2_dt <- eval(parse(text = paste0("m$model$", var2)))
  } 
  
  # Replace the labels
  if (factor_v1 | factor_v2) {
    if (is.null(facet_labs)) facet_labs <- unique(coef_df$value)
    
    if(stats_cp == "ci"){
      facet_labs <- map2(ci_diff, facet_labs, \(aCI, aLevel){
        paste0(aLevel,
               "\n CI(Max - Min): [",
               round(aCI[1], digits = 3),
               ", ",
               round(aCI[2], digits = 3),
               "]")
      }) |> 
        list_c()
    }
    
    coef_df$value <- factor(coef_df$value, labels = facet_labs)
  }
  
  # Plotting the general plot
  if (plot == FALSE) {
    names(coef_df)[1:4] <- c(var1, "coef", "ub", "lb") # just rename the first four cols; the factorial results have a fifth column "value"
    
    return(list(df_coef = coef_df, stats_ci = ci_diff))
  } else {
    aPlot <- interplot.plot(
      m = coef_df,
      hist = hist,
      steps = steps,
      var2_dt = var2_dt,
      predPro = predPro,
      point = point,
      ercolor = ercolor,
      esize = esize,
      ralpha = ralpha,
      rfill = rfill,
      stats_cp = stats_cp,
      txt_caption = txt_caption,
      ci_diff = ci_diff,
      ks_diff = ks_diff,
      ...
    )
  
    
  # Facet for factors
  if (factor_v1 | factor_v2) {
    aPlot <- aPlot + facet_grid(. ~ value)
  }

  return(aPlot)
  }
}

extract_coef_num <- function(
    m,
    m.sims,
    var1,
    var2,
    plot,
    steps,
    ci,
    adjCI,
    predPro,
    var2_vals,
    point,
    xmin,
    xmax
  ){
  
  # Detect if it is a quadratic model
  if (var1 == var2) {
    var12 <- paste0("I(", var1, "^2)")
  } else {var12 <- paste0(var2, ":", var1)}

  # Check if the interaction term is correctly specified ####
  if (!var12 %in% names(m$coef))
    var12 <- paste0(var1, ":", var2)
  # detect the case when the coefficents are named var1:var2 instead of var2:var1
  
  if (!var12 %in% names(m$coef))
    stop(paste(
      "Model does not include the interaction of",
      var1,
      "and",
      var2,
      "."
    ))
  
  # Set the min, max, and steps ####
  if (is.na(xmin)) xmin <- min(m$model[var2], na.rm = T)
  if (is.na(xmax)) xmax <- max(m$model[var2], na.rm = T)
  
  if (is.null(steps)) {
    steps <- eval(parse(text = paste0("length(unique(na.omit(m$model$",var2, ")))")))
  }
  if (steps > 100) steps <- 100  # avoid redundant calculation
  
  coef <- data.frame(
    fake = seq(xmin, xmax, length.out = steps),
    coef1 = NA,
    ub = NA,
    lb = NA
  )
  
  # Calculate the effects ####
  
  ci_diff <- vector(mode = "numeric")
  
  multiplier <- if (var1 == var2) 2 else 1
  
  if (predPro == TRUE) {
    if (is.null(var2_vals))
      stop("The predicted probabilities cannot be estimated without defining 'var2_vals'.")
    
    df <- data.frame(m$model)
    if (sum(grep("X.weights.", names(df))) != 0)
      df <- select(df, -X.weights.) # removed the weights
    df_temp <- select(df, 1) # save the dependent variable separately
    df <- df[-1] %>% # get ride of the dv in case it's a factor
      map(function(var) {
        if (is.factor(var)) {
          model.matrix( ~ var - 1)[, -1] %>%
            # get rid of the first (reference) group
            as.data.frame()
        }
        else{
          as.numeric(var) # in case the initial one is a "labelled" class
        }
      })
    
    
    for (i in seq(df)) {
      # use for loop to avoid the difficulty of flatting list containing vectors and matrices
      if (!is.data.frame(df[[i]])) {
        # keep track the var names
        namesUpdate <- c(names(df_temp), names(df)[[i]])
        df_temp <- cbind(df_temp, df[[i]])
        names(df_temp) <- namesUpdate
      }
      else{
        df_temp <- cbind(df_temp, df[[i]])
      }
    }
    
    df <- df_temp
    
    
    if ("polr" %in% class(m)) {
      #ordered logit does not have intercept in sim
      df <- df[, -1]
    } else{
      names(df)[1] <- "(Intercept)" # replace DV with intercept
      df$`(Intercept)` <- 1
    }
    
    if (var1 == var2) {
      # correct the name of squares
      names(df) <-
        sub("I\\.(.*)\\.2\\.", "I\\(\\1\\^2\\)", names(df))
    }
    
    iv_medians <-
      summarize(df, across(everything(), \(x) median(x, na.rm = TRUE)))
    
    fake_data <-
      iv_medians[rep(1:nrow(iv_medians), each = steps * length(var2_vals)),]
    
    fake_data[[var1]] <-
      with(df, rep(seq(min(get(
        var1
      )), max(get(
        var1
      )), length.out = steps),
      steps = length(var2_vals)))
    fake_data[[var2]] <- rep(var2_vals, each = steps)
    fake_data[[var12]] <- fake_data[[var1]] * fake_data[[var2]]
    
    pp <- rowMeans(plogis(data.matrix(fake_data) %*% t(data.matrix(m.sims@coef))))
    row_quantiles <- function(x, probs) {
      naValue <- NA
      storage.mode(naValue) <- storage.mode(x)
      nrow <- nrow(x)
      q <- matrix(naValue, nrow = nrow, ncol = length(probs))
      if (nrow > 0L) {
        t <- quantile(x[1L,], probs = probs)
        colnames(q) <- names(t)
        q[1L,] <- t
        if (nrow >= 2L) {
          for (rr in 2:nrow) {
            q[rr,] <- quantile(x[rr,], probs = probs)
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
    
    pp_bounds <- row_quantiles(plogis(data.matrix(fake_data) %*% t(data.matrix(m.sims@coef))), prob = c((1 - ci) / 2, 1 - (1 - ci) / 2))
    
    pp <- cbind(pp, pp_bounds)
    pp <- pp * 100
    colnames(pp) <- c("coef1", "lb", "ub")
    pp <- cbind(fake_data[, c(var1, var2)], pp)
    
    
    pp[, var2] <- as.factor(pp[, var2])
    
    names(pp)[1] <- "fake"
    names(pp)[2] <- "value"
    
    coef <- pp
  } else {
    ## Correct marginal effect for quadratic terms
    
    for (i in 1:steps) {
      coef$coef1[i] <- mean(m.sims@coef[, var1] + multiplier * coef$fake[i] * m.sims@coef[, var12])
      coef$ub[i] <- quantile(m.sims@coef[, var1] + multiplier * coef$fake[i] * m.sims@coef[, var12],
                 1 - (1 - ci) / 2)
      coef$lb[i] <- quantile(m.sims@coef[, var1] + multiplier * coef$fake[i] * m.sims@coef[, var12], (1 - ci) / 2)
    }
    
    # Calculate the difference between the effect at the minmum and maxmum value of var2
    
    min_sim <- m.sims@coef[, var1] + multiplier * xmin * m.sims@coef[, var12] # simulation of the value at the minimum value of the conditioning variable
    max_sim <- m.sims@coef[, var1] + multiplier * xmax * m.sims@coef[, var12] # simulation of the value at the maximum value of the conditioning variable
    
    diff <- max_sim - min_sim # calculating the difference
    ci_diff <- c(quantile(diff, (1 - ci) / 2),
                 quantile(diff, 1 - (1 - ci) / 2)) # confidence intervals of the difference
    ks_diff <- ks.test(min_sim, max_sim)
  
  # Correct the standard errors
    if (adjCI == TRUE) {
      ## FDR correction
      coef$sd <- (coef$ub - coef$coef1) / qnorm(1 - (1 - ci) / 2)
      tAdj <-
        fdrInteraction(coef$coef1, coef$sd, df = m$df, level = .95) # calculate critical t
      coef$ub <- coef$coef1 + tAdj * coef$sd
      coef$lb <- coef$coef1 - tAdj * coef$sd
    }  
  }

  return(list(coef, ci_diff, steps))
}

extract_coef_fac <- function(
    factor_v1,
    factor_v2,
    m,
    m.sims,
    var1,
    var2,
    plot,
    steps,
    ci,
    adjCI,
    predPro,
    var2_vals,
    point,
    xmin,
    xmax
  ){
  # Generate the name of the coefficients ####
  if (factor_v1) {
    # var1_bk <- var1
    var1 <- paste0(var1, eval(parse(text = paste0("m$xlevel$", var1))))
  } else if (factor_v2) {
    # var2_bk <- var2
    var2 <- paste0(var2, eval(parse(text = paste0("m$xlevel$", var2))))
  } 
  
  var12 <- paste0(var2, ":", var1)[-1] # the first category is censored to avoid multicolinarity; which is also the rule for lm to deal with factor covariates
  
  # Check if the interaction terms are correctly specified ####
  for (i in seq(var12)) {
    if (!var12[i] %in% names(m$coef))
      var12[i] <- paste0(var1, ":", var2)[-1][i] 
    # detect the case when the coefficents are named var1:var2 instead of var2:var1
    
    if (!var12[i] %in% names(m$coef))
      stop(paste(
        "Model does not include the interaction of",
        var1,
        "and",
        var2,
        "."
      ))
  }
  
  # Set the min, max, and steps ####
  
  if (factor_v2) {
    xmin <- 0
    xmax <- 1
    steps <- 2
  } else {
    if (is.na(xmin)) xmin <- min(m$model[var2], na.rm = T)
    if (is.na(xmax)) xmax <- max(m$model[var2], na.rm = T)
    
    if (is.null(steps)) {
      steps <- eval(parse(text = paste0(
        "length(unique(na.omit(m$model$",
        var2, ")))"
      )))
    }
    if (steps > 100) steps <- 100  # avoid redundant calculation
  }
  
  # Setup the result vectors ####
  
  coef_df <-
    data.frame(
      fake = numeric(0),
      coef1 = numeric(0),
      ub = numeric(0),
      lb = numeric(0),
      model = character(0)
    )
  
  # Calculate the effects ####
  
  ci_diff <- vector(mode = "list")
  
  if (factor_v1) {
    
    for (j in seq(var1)[-length(var1)]) { # remember one category is removed to prevent multicollinearity
      
      coef <- data.frame(
        fake = seq(xmin, xmax, length.out = steps),
        coef1 = NA,
        ub = NA,
        lb = NA
      )
      
      for (i in 1:steps) {
        coef$coef1[i] <- mean(m.sims@coef[, var1[j + 1]] + coef$fake[i] * m.sims@coef[, var12[j]])
        
        coef$ub[i] <- quantile(m.sims@coef[, var1[j + 1]] + coef$fake[i] * m.sims@coef[, var12[j]], 1 - (1 - ci) / 2)
        
        coef$lb[i] <- quantile(m.sims@coef[, var1[j + 1]] + coef$fake[i] * m.sims@coef[, var12[j]], (1 - ci) / 2)
      }
      
      # Calculate the difference between the effect at the minmum and maxmum value of var2
      min_sim <- m.sims@coef[, var1[j + 1]] + xmin * m.sims@coef[, var12[j]] # simulation of the value at the minimum value of the conditioning variable
      max_sim <- m.sims@coef[, var1[j + 1]] + xmax * m.sims@coef[, var12[j]] # simulation of the value at the maximum value of the conditioning variable
      diff <- max_sim - min_sim # calculating the difference
      
      ci_diff[[j]] <- c(
        quantile(diff, (1 - ci) / 2),
        quantile(diff, 1 - (1 - ci) / 2)
      ) # confidence intervals of the difference
      
      # Correct the standard errors
      if (adjCI == TRUE) {
        ## FDR correction
        coef$sd <- (coef$ub - coef$coef1) / qnorm(1 - (1 - ci) / 2)
        tAdj <- fdrInteraction(coef$coef1, coef$sd, df = m$df, level = .95) # calculate critical t
        coef$ub <- coef$coef1 + tAdj * coef$sd
        coef$lb <- coef$coef1 - tAdj * coef$sd
      }
      
      # preparing for later plotting
      coef$value <- var1[j + 1]
      coef_df <- rbind(coef_df, coef)
    }
  } else if (factor_v2) {
    
    for (j in seq(var2)[-length(var2)]) { # remember one category is removed to prevent multicollinearity
      
      coef <- data.frame(
        fake = seq(xmin, xmax, length.out = steps),
        coef1 = NA,
        ub = NA,
        lb = NA
      )
      
      for (i in 1:steps) {
        coef$coef1[i] <- mean(m.sims@coef[, match(var1, names(m$coef))] + coef$fake[i] * m.sims@coef[, match(var12[j], names(m$coef))])
        
        coef$ub[i] <- quantile(m.sims@coef[, match(var1, names(m$coef))] +
                                 coef$fake[i] * m.sims@coef[, match(var12[j], names(m$coef))], 1 - (1 - ci) / 2)
        coef$lb[i] <- quantile(m.sims@coef[, match(var1, names(m$coef))] + coef$fake[i] * m.sims@coef[, match(var12[j], names(m$coef))], (1 - ci) / 2)
      }
      
      # Calculate the difference between the effect at the minmum and maxmum value of var2
      min_sim <- m.sims@coef[, var1] + xmin * m.sims@coef[, var12[j]] # simulation of the value at the minimum value of the conditioning variable
      max_sim <- m.sims@coef[, var1] + xmax * m.sims@coef[, var12[j]] # simulation of the value at the maximum value of the conditioning variable
      diff <- max_sim - min_sim # calculating the difference
      
      ci_diff[[j]] <- c(
        quantile(diff, (1 - ci) / 2),
        quantile(diff, 1 - (1 - ci) / 2)
      ) # confidence intervals of the difference
      
      # Correct the standard errors
      if (adjCI == TRUE) {
        ## FDR correction
        coef$sd <- (coef$ub - coef$coef1) / qnorm(1 - (1 - ci) / 2)
        tAdj <- fdrInteraction(coef$coef1, coef$sd, df = m$df, level = .95) # calculate critical t
        coef$ub <- coef$coef1 + tAdj * coef$sd
        coef$lb <- coef$coef1 - tAdj * coef$sd
      }
      
      coef$value <- var2[j + 1] #name of the level2 in var2
      coef_df <- rbind(coef_df, coef)
      
    }
  }
  
  names(ci_diff) <- var12
  
  return(list(coef_df, ci_diff, steps))
} 

