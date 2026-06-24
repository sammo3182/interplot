if(getRversion() >= "2.15.1") utils::globalVariables(c(".", "X.weights."))

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
#' @param var3 An optional name (as a string) of a third variable for a three-way interaction \code{var1 * var2 * var3}. When supplied, the conditional effect of \code{var1} across \code{var2} is shown at several values/levels of \code{var3}. The default \code{NULL} gives the standard two-way behavior. Requires continuous \code{var1} and \code{var2}.
#' @param var3_vals An optional numeric vector giving the values of a continuous \code{var3} to condition on. The default is the mean and the mean plus or minus one standard deviation. Ignored when \code{var3} is a factor.
#' @param facet A logical value, used only with \code{var3}. \code{TRUE} (default) draws one panel per value/level of \code{var3}; \code{FALSE} overlays the curves colored by \code{var3}.
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
#' @importFrom stats quantile qnorm median plogis model.matrix ks.test sd setNames
#' @importFrom purrr map map2 list_c list_rbind
#' @importFrom interactionTest fdrInteraction
#' @import ggplot2
#' @import dplyr
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
#' URL: \url{https://jee3.web.rice.edu/interaction-overconfidence.pdf}.
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
                              var3 = NULL,
                              var3_vals = NULL,
                              facet = TRUE,
                              ...) {

  m.class <- class(m)

  if (predPro == TRUE & inherits(m, "lm") & !inherits(m, "glm"))
    stop("Predicted probability is estimated only for general linear models.")

  # Coefficient data frame ####
  ## Factorial base terms
  factor_v1 <- is.factor(m$model[[var1]])
  factor_v2 <- is.factor(m$model[[var2]])

  if (factor_v1 & factor_v2)
    stop("The function does not support interactions between two factors.")

  if ((factor_v1 | factor_v2) & predPro == TRUE)
    stop("The current version does not support estimating predicted probabilities for factor base terms.")

  is_3way <- !is.null(var3)
  is_nonlinear <- !is_3way && !factor_v1 && !factor_v2 && predPro == FALSE &&
    var1 != var2 && detect_nonlinear(stats::terms(m), var1, var2)

  # The nonlinear path draws its own coefficients (arm::sim drops spline columns);
  # every other path uses arm::sim.
  m.sims <- if (is_nonlinear) NULL else arm::sim(m, sims)

  if (is_3way) {
    if (predPro == TRUE)
      stop("Predicted probabilities are not supported for three-way interactions.")
    if (var1 == var2 || var1 == var3 || var2 == var3)
      stop("var1, var2, and var3 must be three distinct variables.")
    ls_results <- extract_coef_3way(
      draws = m.sims@coef,
      frame = m$model,
      var1 = var1,
      var2 = var2,
      var3 = var3,
      var3_vals = var3_vals,
      ci = ci,
      adjCI = adjCI,
      df_resid = m$df,
      steps = steps,
      xmin = xmin,
      xmax = xmax
    )
  } else if (is_nonlinear) {
    ls_results <- extract_coef_nonlinear(
      tt = stats::delete.response(stats::terms(m)),
      data_full = me_model_data(m, fallback = m$model),
      draws = sim_betas_mvn(stats::coef(m), stats::vcov(m), sims),
      var1 = var1,
      var2 = var2,
      ci = ci,
      adjCI = adjCI,
      df_resid = m$df,
      steps = steps,
      xmin = xmin,
      xmax = xmax
    )
  } else if (factor_v1 | factor_v2) {
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
  ks_diff <- ls_results[[4]]
  
  # Plotting ####
  
  if (hist == TRUE & all(is.na(var2_dt))) {
    var2_dt <- m$model[[var2]]
  }
  
  # Replace the labels
  if (is_3way) {
    # The "value" column already carries ordered var3 labels. For stats_cp = "ci"
    # the per-level CI(Max - Min) goes to the caption (survives faceting/overlay).
    if (stats_cp == "ci") {
      ci_note <- map2(ci_diff, levels(coef_df$value), \(aCI, aLevel) {
        paste0(aLevel, " CI(Max - Min): [",
               round(aCI[1], digits = 3), ", ",
               round(aCI[2], digits = 3), "]")
      }) |>
        list_c() |>
        paste(collapse = "\n")
      txt_caption <- paste0(ci_note, txt_caption)
    }
  } else if (factor_v1 | factor_v2) {
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
    names(coef_df)[1:4] <- c(var1, "coef", "ub", "lb") # just rename the first four cols; the factorial/three-way results have a fifth column "value"

    return(list(df_coef = coef_df, stats_ci = ci_diff))
  } else {
    # For a three-way overlay (facet = FALSE) color the curves by var3.
    overlay <- is_3way && !facet

    aPlot <- interplot.plot(
      m = coef_df,
      var1 = var1,
      var2 = var2,
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
      overlay = overlay,
      ...
    )


  # Facet for factors or three-way small multiples
  if (factor_v1 | factor_v2) {
    aPlot <- aPlot + facet_grid(. ~ value)
  } else if (is_3way && facet) {
    aPlot <- aPlot + facet_wrap(~ value)
  } else if (is_3way && !facet) {
    aPlot <- aPlot + labs(colour = var3, fill = var3)
  }

  return(aPlot)
  }
}

# Family-agnostic three-way core. `draws` is a (sims x coef) matrix with column
# names; `frame` is a data frame holding var1/var2/var3; `df_resid` enables the
# adjCI false-discovery correction (NULL to disable, e.g. for mixed models).
extract_coef_3way <- function(
    draws,
    frame,
    var1,
    var2,
    var3,
    var3_vals,
    ci,
    adjCI,
    df_resid,
    steps,
    xmin,
    xmax
  ){

  if (is.factor(frame[[var1]]) || is.factor(frame[[var2]]))
    stop("Three-way interaction plots currently require continuous var1 and var2.")

  coef_names <- colnames(draws)

  # Two-way pieces of the var1 marginal effect (b1 + b12 * var2).
  b12_nm <- resolve_term(c(var1, var2), coef_names)
  if (is.na(b12_nm))
    stop("Model does not include the interaction of ", var1, " and ", var2, ".")
  sim_b1  <- draws[, var1]
  sim_b12 <- draws[, b12_nm]

  # var2 sequence (x-axis).
  if (is.na(xmin)) xmin <- min(frame[[var2]], na.rm = TRUE)
  if (is.na(xmax)) xmax <- max(frame[[var2]], na.rm = TRUE)
  if (is.null(steps)) steps <- length(unique(na.omit(frame[[var2]])))
  if (steps > 100) steps <- 100
  fake <- seq(xmin, xmax, length.out = steps)

  # Build one (label, shifted intercept draws, shifted slope draws) spec per
  # value/level of var3. Fixing var3 = c turns the three-way effect into the
  # two-way problem with intercept (b1 + b13*c) and slope (b12 + b123*c).
  factor_v3 <- is.factor(frame[[var3]])

  if (factor_v3) {
    lvls <- levels(frame[[var3]])
    specs <- map(lvls, \(L) {
      if (L == lvls[1]) { # reference level: no shift
        list(label = paste0(var3, " = ", L), sv1 = sim_b1, sv12 = sim_b12)
      } else {
        b13_nm  <- resolve_term(c(var1, paste0(var3, L)), coef_names)
        b123_nm <- resolve_term(c(var1, var2, paste0(var3, L)), coef_names)
        if (is.na(b13_nm) || is.na(b123_nm))
          stop("Model does not include the three-way interaction of ", var1, ", ", var2, ", and ", var3, ".")
        list(label = paste0(var3, " = ", L),
             sv1 = sim_b1 + draws[, b13_nm],
             sv12 = sim_b12 + draws[, b123_nm])
      }
    })
  } else {
    if (is.null(var3_vals)) {
      mu  <- mean(frame[[var3]], na.rm = TRUE)
      sdv <- sd(frame[[var3]], na.rm = TRUE)
      var3_vals <- c(mu - sdv, mu, mu + sdv)
      labs3 <- paste0(var3, c(" = Low (-1 SD)", " = Mean", " = High (+1 SD)"))
    } else {
      labs3 <- paste0(var3, " = ", round(var3_vals, 3))
    }
    b13_nm  <- resolve_term(c(var1, var3), coef_names)
    b123_nm <- resolve_term(c(var1, var2, var3), coef_names)
    if (is.na(b13_nm) || is.na(b123_nm))
      stop("Model does not include the three-way interaction of ", var1, ", ", var2, ", and ", var3, ".")
    sim_b13  <- draws[, b13_nm]
    sim_b123 <- draws[, b123_nm]
    specs <- map2(var3_vals, labs3, \(c3, lab) {
      list(label = lab, sv1 = sim_b1 + c3 * sim_b13, sv12 = sim_b12 + c3 * sim_b123)
    })
  }

  do_adj <- isTRUE(adjCI) && !is.null(df_resid)
  if (isTRUE(adjCI) && is.null(df_resid))
    warning("adjCI is not available for three-way interactions in this model class; showing unadjusted intervals.")

  labels <- vapply(specs, \(s) s$label, character(1))

  results <- map(specs, \(s) {
    coef <- sim_coef_vec(s$sv1, s$sv12, fake, ci)
    d <- sim_diff_stats(s$sv1, s$sv12, xmin, xmax, ci)

    if (do_adj) {
      coef$sd <- (coef$ub - coef$coef1) / qnorm(1 - (1 - ci) / 2)
      tAdj <- fdrInteraction(coef$coef1, coef$sd, df = df_resid, level = ci)
      coef$ub <- coef$coef1 + tAdj * coef$sd
      coef$lb <- coef$coef1 - tAdj * coef$sd
    }

    coef$value <- s$label
    list(coef = coef, ci_diff = d$ci_diff)
  })

  coef_df <- map(results, "coef") |> list_rbind()
  coef_df$value <- factor(coef_df$value, levels = labels)
  ci_diff <- map(results, "ci_diff") |> setNames(labels)

  list(coef_df, ci_diff, steps, NULL)
}


# Family-agnostic nonlinear core. `tt` is the response-deleted (fixed-effects)
# terms object; `data_full` the original model data; `draws` the coefficient
# draws (rows = draws, named columns); `df_resid` enables adjCI (NULL disables).
extract_coef_nonlinear <- function(
    tt,
    data_full,
    draws,
    var1,
    var2,
    ci,
    adjCI,
    df_resid,
    steps,
    xmin,
    xmax
  ){

  v2_vals <- data_full[[var2]]
  if (is.null(v2_vals))
    stop("Cannot find values of '", var2, "' to set the plotting range. Refit the model with the data available.")

  if (is.na(xmin)) xmin <- min(v2_vals, na.rm = TRUE)
  if (is.na(xmax)) xmax <- max(v2_vals, na.rm = TRUE)
  if (is.null(steps)) steps <- length(unique(na.omit(v2_vals)))
  if (steps > 100) steps <- 100
  fake <- seq(xmin, xmax, length.out = steps)

  des <- build_me_design(tt, data_full, var1, var2, fake)

  # Guard against rank-deficient (aliased, NA) coefficients, which are absent
  # from the draws; drop their terms from the marginal-effect basis.
  valid <- des$term_names %in% colnames(draws)
  if (!all(valid)) {
    warning("Dropping ", sum(!valid),
            " rank-deficient (aliased) term(s) from the marginal effect of '", var1, "'.")
    des$term_names <- des$term_names[valid]
    des$G <- des$G[, valid, drop = FALSE]
  }

  sim_mat <- draws[, des$term_names, drop = FALSE]

  coef <- sim_coef_grad(sim_mat, des$G, fake, ci)
  diff_stats <- sim_diff_stats_grad(sim_mat, des$G, ci)
  ci_diff <- diff_stats$ci_diff
  ks_diff <- diff_stats$ks_diff

  do_adj <- isTRUE(adjCI) && !is.null(df_resid)
  if (isTRUE(adjCI) && is.null(df_resid))
    warning("adjCI is not available for nonlinear effects in this model class; showing unadjusted intervals.")
  if (do_adj) {
    coef$sd <- (coef$ub - coef$coef1) / qnorm(1 - (1 - ci) / 2)
    tAdj <- fdrInteraction(coef$coef1, coef$sd, df = df_resid, level = ci)
    coef$ub <- coef$coef1 + tAdj * coef$sd
    coef$lb <- coef$coef1 - tAdj * coef$sd
  }

  list(coef, ci_diff, steps, ks_diff)
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
  if (is.na(xmin)) xmin <- min(m$model[[var2]], na.rm = TRUE)
  if (is.na(xmax)) xmax <- max(m$model[[var2]], na.rm = TRUE)

  if (is.null(steps)) {
    steps <- length(unique(na.omit(m$model[[var2]])))
  }
  if (steps > 100) steps <- 100

  fake <- seq(xmin, xmax, length.out = steps)

  # Calculate the effects ####

  ci_diff <- numeric(0)
  ks_diff <- NULL
  multiplier <- if (var1 == var2) 2 else 1

  if (predPro == TRUE) {
    if (is.null(var2_vals))
      stop("The predicted probabilities cannot be estimated without defining 'var2_vals'.")

    df <- data.frame(m$model)
    if (any(grepl("X.weights.", names(df))))
      df <- select(df, -X.weights.)
    df_temp <- select(df, 1)
    df <- df[-1] |>
      map(\(v) {
        if (is.factor(v)) {
          as.data.frame(model.matrix(~ v - 1)[, -1, drop = FALSE])
        } else {
          as.numeric(v)
        }
      })

    for (i in seq(df)) {
      if (!is.data.frame(df[[i]])) {
        namesUpdate <- c(names(df_temp), names(df)[[i]])
        df_temp <- cbind(df_temp, df[[i]])
        names(df_temp) <- namesUpdate
      } else {
        df_temp <- cbind(df_temp, df[[i]])
      }
    }
    df <- df_temp

    if (inherits(m, "polr")) {
      df <- df[, -1]
    } else {
      names(df)[1] <- "(Intercept)"
      df$`(Intercept)` <- 1
    }

    if (var1 == var2) {
      names(df) <- sub("I\\.(.*)\\.2\\.", "I\\(\\1\\^2\\)", names(df))
    }

    iv_medians <- summarize(df, across(everything(), \(x) median(x, na.rm = TRUE)))
    fake_data <- iv_medians[rep(1, each = steps * length(var2_vals)), ]
    fake_data[[var1]] <- rep(
      seq(min(df[[var1]]), max(df[[var1]]), length.out = steps),
      times = length(var2_vals)
    )
    fake_data[[var2]] <- rep(var2_vals, each = steps)
    fake_data[[var12]] <- fake_data[[var1]] * fake_data[[var2]]

    linpred <- data.matrix(fake_data) %*% t(data.matrix(m.sims@coef))
    pp_mat <- plogis(linpred)
    probs <- c((1 - ci) / 2, 1 - (1 - ci) / 2)

    coef <- data.frame(
      fake = fake_data[[var1]],
      value = as.factor(fake_data[[var2]]),
      coef1 = rowMeans(pp_mat) * 100,
      lb = apply(pp_mat, 1, quantile, probs = probs[1]) * 100,
      ub = apply(pp_mat, 1, quantile, probs = probs[2]) * 100
    )
  } else {
    sim_v1 <- m.sims@coef[, var1]
    sim_v12 <- m.sims@coef[, var12]

    coef <- sim_coef_vec(sim_v1, sim_v12, fake, ci, multiplier)

    diff_stats <- sim_diff_stats(sim_v1, sim_v12, xmin, xmax, ci, multiplier)
    ci_diff <- diff_stats$ci_diff
    ks_diff <- diff_stats$ks_diff

    if (adjCI == TRUE) {
      coef$sd <- (coef$ub - coef$coef1) / qnorm(1 - (1 - ci) / 2)
      tAdj <- fdrInteraction(coef$coef1, coef$sd, df = m$df, level = .95)
      coef$ub <- coef$coef1 + tAdj * coef$sd
      coef$lb <- coef$coef1 - tAdj * coef$sd
    }
  }

  return(list(coef, ci_diff, steps, ks_diff))
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
    var1 <- paste0(var1, m$xlevel[[var1]])
  } else if (factor_v2) {
    var2 <- paste0(var2, m$xlevel[[var2]])
  }

  var12 <- paste0(var2, ":", var1)[-1]

  # Check if the interaction terms are correctly specified ####
  for (i in seq(var12)) {
    if (!var12[i] %in% names(m$coef))
      var12[i] <- paste0(var1, ":", var2)[-1][i]
    if (!var12[i] %in% names(m$coef))
      stop(paste("Model does not include the interaction of", var1, "and", var2, "."))
  }

  # Set the min, max, and steps ####
  if (factor_v2) {
    xmin <- 0
    xmax <- 1
    steps <- 2
  } else {
    if (is.na(xmin)) xmin <- min(m$model[[var2]], na.rm = TRUE)
    if (is.na(xmax)) xmax <- max(m$model[[var2]], na.rm = TRUE)
    if (is.null(steps)) {
      steps <- length(unique(na.omit(m$model[[var2]])))
    }
    if (steps > 100) steps <- 100
  }

  fake_seq <- seq(xmin, xmax, length.out = steps)

  # Calculate the effects ####
  ci_diff <- vector(mode = "list")

  if (factor_v1) {
    results <- map(seq(var1)[-length(var1)], \(j) {
      sim_v1 <- m.sims@coef[, var1[j + 1]]
      sim_v12 <- m.sims@coef[, var12[j]]
      coef <- sim_coef_vec(sim_v1, sim_v12, fake_seq, ci)
      d <- sim_diff_stats(sim_v1, sim_v12, xmin, xmax, ci)

      if (adjCI == TRUE) {
        coef$sd <- (coef$ub - coef$coef1) / qnorm(1 - (1 - ci) / 2)
        tAdj <- fdrInteraction(coef$coef1, coef$sd, df = m$df, level = .95)
        coef$ub <- coef$coef1 + tAdj * coef$sd
        coef$lb <- coef$coef1 - tAdj * coef$sd
      }

      coef$value <- var1[j + 1]
      list(coef = coef, ci_diff = d$ci_diff)
    })

    coef_df <- map(results, "coef") |> list_rbind()
    ci_diff <- map(results, "ci_diff") |> setNames(var12)

  } else if (factor_v2) {
    results <- map(seq(var2)[-length(var2)], \(j) {
      sim_v1 <- m.sims@coef[, match(var1, names(m$coef))]
      sim_v12 <- m.sims@coef[, match(var12[j], names(m$coef))]
      coef <- sim_coef_vec(sim_v1, sim_v12, fake_seq, ci)
      d <- sim_diff_stats(sim_v1, sim_v12, xmin, xmax, ci)

      if (adjCI == TRUE) {
        coef$sd <- (coef$ub - coef$coef1) / qnorm(1 - (1 - ci) / 2)
        tAdj <- fdrInteraction(coef$coef1, coef$sd, df = m$df, level = .95)
        coef$ub <- coef$coef1 + tAdj * coef$sd
        coef$lb <- coef$coef1 - tAdj * coef$sd
      }

      coef$value <- var2[j + 1]
      list(coef = coef, ci_diff = d$ci_diff)
    })

    coef_df <- map(results, "coef") |> list_rbind()
    ci_diff <- map(results, "ci_diff") |> setNames(var12)
  }

  return(list(coef_df, ci_diff, steps, NULL))
} 

