if(getRversion() >= "2.15.1") utils::globalVariables(c(".", "value", ":="))

#' Plot Conditional Coefficients in Bayesian Models with Interaction Terms
#'
#' \code{interplot.brmsfit} is a method to calculate conditional coefficient
#' estimates from the posterior draws of a Bayesian regression model fitted with
#' \code{\link[brms]{brm}} that includes a two-way interaction term.
#'
#' @param m A model object of class \code{brmsfit} including an interaction term.
#' @param var1 The name (as a string) of the variable of interest in the interaction term; its conditional coefficient estimates will be plotted.
#' @param var2 The name (as a string) of the other variable in the interaction term.
#' @param plot A logical value indicating whether the output is a plot or a dataframe including the conditional coefficient estimates of var1, their upper and lower bounds, and the corresponding values of var2.
#' @param steps Desired length of the sequence. A non-negative number, which for seq and seq.int will be rounded up if fractional. The default is 100 or the unique categories in the \code{var2} (when it is less than 100. Also see \code{\link{unique}}).
#' @param ci A numeric value defining the credible interval. The default value is 95\% (0.95). For \code{brmsfit} objects the interval summarizes the posterior draws, so it is an (equal-tailed) Bayesian credible interval rather than a frequentist confidence interval.
#' @param adjCI The false-discovery-rate adjustment of Esarey and Sumner (2017) is a frequentist correction and does not apply to Bayesian posteriors. The argument is ignored (with a warning) for \code{brmsfit} objects.
#' @param hist A logical value indicating if there is a histogram of `var2` added at the bottom of the conditional effect plot.
#' @param var2_dt A numerical value indicating the frequency distribution of `var2`. It is only used when `hist == TRUE`. When the object is a model, the default is the distribution of `var2` of the model.
#' @param predPro A logical value with default of `FALSE`. When the `m` is fitted with a Bernoulli or binomial family and the argument is set to `TRUE`, the function plots the posterior expected predicted probabilities at the values given by `var2_vals`, computed with \code{\link[brms]{posterior_epred}}.
#' @param var2_vals A numerical value indicating the values the predicted probabilities are estimated, when `predPro` is `TRUE`.
#' @param point A logical value determining the format of plot. By default, the function produces a line plot when var2 takes on ten or more distinct values and a point (dot-and-whisker) plot otherwise; option TRUE forces a point plot.
#' @param sims Ignored for \code{brmsfit} objects: the posterior draws produced by the sampler are used directly instead of re-simulating the coefficients.
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
#' @details \code{interplot.brmsfit} is an S3 method of \code{interplot} for models fitted with \code{\link[brms]{brm}}. Unlike the frequentist methods, it does not call \code{arm::sim}: the posterior draws of the population-level (fixed) effects are extracted directly with \code{as.matrix} and the conditional coefficient \eqn{b_{var1} + b_{var1:var2} \cdot var2} is computed for every draw. Point estimates are posterior means and the bounds are posterior quantiles.
#'
#' Because the output function is based on \code{\link[ggplot2]{ggplot}}, any additional arguments and layers supported by \code{ggplot2} can be added with the \code{+}.
#'
#' The 'brms' package is only suggested by 'interplot'; it must be installed for this method to run.
#'
#' @return The function returns a \code{ggplot} object, or a list with the data frame of conditional coefficients when \code{plot = FALSE}.
#'
#' @examples
#' \dontrun{
#' library(brms)
#' data(mtcars)
#'
#' # A Bayesian linear model with a two-way interaction
#' m_brms <- brm(mpg ~ wt * cyl, data = mtcars, chains = 2, refresh = 0)
#'
#' # Identical interface; the band is a 95% posterior credible interval
#' interplot(m_brms, var1 = "cyl", var2 = "wt")
#'
#' # Posterior predicted probabilities for a Bernoulli model
#' m_brms_bin <- brm(am ~ wt * cyl, data = mtcars,
#'                   family = bernoulli(), chains = 2, refresh = 0)
#' interplot(m_brms_bin, var1 = "wt", var2 = "cyl",
#'           predPro = TRUE, var2_vals = c(4, 6, 8))
#' }
#'
#' @importFrom stats quantile median setNames
#' @importFrom purrr map map2 list_c list_rbind
#' @import ggplot2
#' @import dplyr
#'
#' @export


interplot.brmsfit <- function(m,
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

  if (!requireNamespace("brms", quietly = TRUE))
    stop("Plotting `brmsfit` objects requires the 'brms' package. Install it with install.packages('brms').")

  if (isTRUE(adjCI))
    warning("`adjCI` is a frequentist false-discovery-rate correction and is ignored for Bayesian (brmsfit) models.")

  # Posterior draws of the population-level coefficients (already sampled; no re-simulation).
  draws <- as.matrix(m)
  mdata <- m$data

  # Coefficient data frame ####
  ## Factorial base terms
  factor_v1 <- is.factor(mdata[[var1]])
  factor_v2 <- is.factor(mdata[[var2]])

  if (factor_v1 & factor_v2)
    stop("The function does not support interactions between two factors.")

  if ((factor_v1 | factor_v2) & predPro == TRUE)
    stop("The current version does not support estimating predicted probabilities for factor base terms.")

  is_3way <- !is.null(var3)

  # Nonlinear-in-moderator (e.g. var1 * I(var2^2), poly/spline of var2). Detected
  # from the brms fixed-effects terms; only for numeric base terms, no predPro.
  brms_fe_terms <- stats::delete.response(stats::terms(brms::brmsterms(m$formula)$dpars$mu$fe))
  is_nonlinear <- !is_3way && !factor_v1 && !factor_v2 && predPro == FALSE &&
    var1 != var2 && detect_nonlinear(brms_fe_terms, var1, var2)

  if (is_3way) {
    if (predPro == TRUE)
      stop("Predicted probabilities are not supported for three-way interactions.")
    if (var1 == var2 || var1 == var3 || var2 == var3)
      stop("var1, var2, and var3 must be three distinct variables.")

    # Family-agnostic three-way core expects plain coefficient names (no brms
    # `b_` prefix): keep the population-level columns and strip the prefix.
    bcols <- startsWith(colnames(draws), "b_")
    draws3 <- draws[, bcols, drop = FALSE]
    colnames(draws3) <- sub("^b_", "", colnames(draws3))

    ls_results <- extract_coef_3way(
      draws = draws3,
      frame = mdata,
      var1 = var1,
      var2 = var2,
      var3 = var3,
      var3_vals = var3_vals,
      ci = ci,
      adjCI = adjCI,
      df_resid = NULL,
      steps = steps,
      xmin = xmin,
      xmax = xmax
    )
  } else if (is_nonlinear) {
    ls_results <- extract_coef_nonlinearB(
      m = m,
      draws = draws,
      tt = brms_fe_terms,
      var1 = var1,
      var2 = var2,
      ci = ci,
      steps = steps,
      xmin = xmin,
      xmax = xmax
    )
  } else if (factor_v1 | factor_v2) {
    ls_results <- extract_coef_facB(
      factor_v1 = factor_v1,
      factor_v2 = factor_v2,
      m = m,
      draws = draws,
      var1 = var1,
      var2 = var2,
      ci = ci,
      steps = steps,
      xmin = xmin,
      xmax = xmax
    )
  } else {
    ls_results <- extract_coef_numB(
      m = m,
      draws = draws,
      var1 = var1,
      var2 = var2,
      ci = ci,
      steps = steps,
      predPro = predPro,
      var2_vals = var2_vals,
      xmin = xmin,
      xmax = xmax
    )
  }

  coef_df <- ls_results[[1]]
  ci_diff <- ls_results[[2]]
  steps   <- ls_results[[3]]
  ks_diff <- ls_results[[4]]

  # Plotting ####
  if (hist == TRUE & all(is.na(var2_dt)))
    var2_dt <- mdata[[var2]]

  # Replace the labels (factor base term). The CI(Max - Min) note goes to the
  # caption (always visible) rather than the facet strip.
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

    if (stats_cp == "ci") {
      ci_note <- map2(ci_diff, facet_labs, \(aCI, aLevel) {
        paste0(if (length(facet_labs) > 1) paste0(aLevel, " ") else "",
               "CI(Max - Min): [",
               round(aCI[1], digits = 3), ", ",
               round(aCI[2], digits = 3), "]")
      }) |>
        list_c() |>
        paste(collapse = "\n")
      txt_caption <- paste0(ci_note, txt_caption)
    }

    coef_df$value <- factor(coef_df$value, labels = facet_labs)
  }

  # Plotting the general plot
  if (plot == FALSE) {
    names(coef_df)[1:4] <- c(var1, "coef", "ub", "lb") # rename the first four cols; factorial results keep a fifth column "value"

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


# Resolve a draw-matrix column name for a two-way interaction, trying both
# orderings of the terms (brms preserves the formula order, but be defensive).
resolve_brms_int <- function(draw_names, a, b) {
  cand <- paste0("b_", c(paste0(a, ":", b), paste0(b, ":", a)))
  hit <- cand[cand %in% draw_names]
  if (length(hit)) hit[1] else NA_character_
}


extract_coef_numB <- function(
    m,
    draws,
    var1,
    var2,
    ci,
    steps,
    predPro,
    var2_vals,
    xmin,
    xmax
  ){

  mdata <- m$data
  draw_names <- colnames(draws)

  # Predicted-probability path (Bayesian posterior_epred) ####
  if (predPro == TRUE) {
    if (is.null(var2_vals))
      stop("The predicted probabilities cannot be estimated without defining 'var2_vals'.")

    n_steps <- if (is.null(steps)) 100 else steps

    v1_rng <- range(mdata[[var1]], na.rm = TRUE)
    v1_seq <- seq(v1_rng[1], v1_rng[2], length.out = n_steps)
    grid <- expand.grid(.v1 = v1_seq, .v2 = var2_vals)

    # Representative profile: median for numeric, modal level for factors.
    rep_row <- map(mdata, \(x) {
      if (is.numeric(x)) median(x, na.rm = TRUE)
      else factor(names(which.max(table(x))), levels = levels(factor(x)))
    })
    fake_data <- as.data.frame(rep_row, stringsAsFactors = FALSE)[rep(1, nrow(grid)), , drop = FALSE]
    fake_data[[var1]] <- grid$.v1
    fake_data[[var2]] <- grid$.v2

    pp <- brms::posterior_epred(m, newdata = fake_data, re_formula = NA)
    probs <- c((1 - ci) / 2, 1 - (1 - ci) / 2)

    # Keep "value" as the last column so the positional plot = FALSE rename
    # (first four columns -> var1, coef, ub, lb) leaves it intact.
    coef <- data.frame(
      fake  = grid$.v1,
      coef1 = colMeans(pp) * 100,
      ub    = apply(pp, 2, quantile, probs = probs[2]) * 100,
      lb    = apply(pp, 2, quantile, probs = probs[1]) * 100,
      value = as.factor(grid$.v2)
    )

    return(list(coef, numeric(0), n_steps, NULL))
  }

  # Conditional-coefficient path ####
  bvar1 <- paste0("b_", var1)

  # Detect if it is a quadratic model
  if (var1 == var2) {
    bvar12 <- draw_names[grepl(paste0("^b_I.*", var1, ".*2"), draw_names)][1]
    multiplier <- 2
  } else {
    bvar12 <- resolve_brms_int(draw_names, var1, var2)
    multiplier <- 1
  }

  if (!bvar1 %in% draw_names)
    stop(paste0("Model does not include a population-level coefficient for '", var1, "'."))
  if (is.na(bvar12))
    stop(paste0("Model does not include the interaction of ", var1, " and ", var2,
                ". Available population-level terms: ",
                paste(draw_names[startsWith(draw_names, "b_")], collapse = ", "), "."))

  # Set the min, max, and steps ####
  if (is.na(xmin)) xmin <- min(mdata[[var2]], na.rm = TRUE)
  if (is.na(xmax)) xmax <- max(mdata[[var2]], na.rm = TRUE)
  if (is.null(steps)) steps <- length(unique(na.omit(mdata[[var2]])))
  if (steps > 100) steps <- 100
  fake <- seq(xmin, xmax, length.out = steps)

  # Calculate the effects ####
  sim_v1  <- draws[, bvar1]
  sim_v12 <- draws[, bvar12]

  coef <- sim_coef_vec(sim_v1, sim_v12, fake, ci, multiplier)
  diff_stats <- sim_diff_stats(sim_v1, sim_v12, xmin, xmax, ci, multiplier)

  list(coef, diff_stats$ci_diff, steps, diff_stats$ks_diff)
}


extract_coef_facB <- function(
    factor_v1,
    factor_v2,
    m,
    draws,
    var1,
    var2,
    ci,
    steps,
    xmin,
    xmax
  ){

  mdata <- m$data
  draw_names <- colnames(draws)

  if (factor_v1) {
    # var1 is the factor being plotted; var2 (numeric) is the x-axis.
    lvls <- levels(mdata[[var1]])[-1] # non-reference levels

    if (is.na(xmin)) xmin <- min(mdata[[var2]], na.rm = TRUE)
    if (is.na(xmax)) xmax <- max(mdata[[var2]], na.rm = TRUE)
    if (is.null(steps)) steps <- length(unique(na.omit(mdata[[var2]])))
    if (steps > 100) steps <- 100
    fake_seq <- seq(xmin, xmax, length.out = steps)

    results <- map(lvls, \(L) {
      base  <- paste0("b_", var1, L)
      inter <- resolve_brms_int(draw_names, var2, paste0(var1, L))
      if (!base %in% draw_names || is.na(inter))
        stop(paste("Model does not include the interaction of", var1, "and", var2, "."))

      coef <- sim_coef_vec(draws[, base], draws[, inter], fake_seq, ci)
      d <- sim_diff_stats(draws[, base], draws[, inter], xmin, xmax, ci)
      coef$value <- paste0(var1, L)
      list(coef = coef, ci_diff = d$ci_diff)
    })

  } else {
    # var2 is the factor (x-axis levels 0/1); var1 (numeric) is plotted.
    lvls <- levels(mdata[[var2]])[-1]
    xmin <- 0
    xmax <- 1
    steps <- 2
    fake_seq <- seq(xmin, xmax, length.out = steps)

    base <- paste0("b_", var1)
    if (!base %in% draw_names)
      stop(paste0("Model does not include a population-level coefficient for '", var1, "'."))

    results <- map(lvls, \(L) {
      inter <- resolve_brms_int(draw_names, paste0(var2, L), var1)
      if (is.na(inter))
        stop(paste("Model does not include the interaction of", var1, "and", var2, "."))

      coef <- sim_coef_vec(draws[, base], draws[, inter], fake_seq, ci)
      d <- sim_diff_stats(draws[, base], draws[, inter], xmin, xmax, ci)
      coef$value <- paste0(var2, L)
      list(coef = coef, ci_diff = d$ci_diff)
    })
  }

  coef_df <- map(results, "coef") |> list_rbind()
  ci_diff <- map(results, "ci_diff")

  list(coef_df, ci_diff, steps, NULL)
}


# Nonlinear-in-moderator effect for brmsfit (e.g. var1 * I(var2^2), poly/spline
# of var2). brms mangles transformed terms in its `b_` coefficient names (e.g.
# `b_wt:IhpE2` for `wt:I(hp^2)`), which is awkward to parse. Instead we rebuild
# the marginal-effect gradient from the *clean* fixed-effects terms via
# `model.matrix` (which regenerates spline/polynomial bases through predvars),
# then map the clean design columns onto the brms-mangled draw columns by
# position: `model.matrix(tt, data)` and the population-level `b_*` draws share
# the same term order, so the i-th design column corresponds to the i-th `b_`
# coefficient. The remaining work reuses the family-agnostic gradient helpers.
extract_coef_nonlinearB <- function(
    m,
    draws,
    tt,
    var1,
    var2,
    ci,
    steps,
    xmin,
    xmax
  ){

  mdata <- m$data

  # Population-level draws with the `b_` prefix stripped.
  bcols <- startsWith(colnames(draws), "b_")
  b <- draws[, bcols, drop = FALSE]
  colnames(b) <- sub("^b_", "", colnames(b))

  v2_vals <- mdata[[var2]]
  if (is.na(xmin)) xmin <- min(v2_vals, na.rm = TRUE)
  if (is.na(xmax)) xmax <- max(v2_vals, na.rm = TRUE)
  if (is.null(steps)) steps <- length(unique(na.omit(v2_vals)))
  if (steps > 100) steps <- 100
  fake <- seq(xmin, xmax, length.out = steps)

  # Clean -> brms positional name map. The full design and the `b_` draws list
  # the same terms in the same order (intercept first), so align by position.
  X <- stats::model.matrix(tt, mdata)
  clean <- colnames(X)
  clean[clean == "(Intercept)"] <- "Intercept"
  if (length(clean) != ncol(b))
    stop("Cannot align the design matrix with the brms coefficients for the nonlinear effect of '",
         var1, "'. Use brms-native marginal effects (e.g. marginaleffects) instead.")
  name_map <- setNames(colnames(b), clean)

  # Gradient basis for the var1 marginal effect from the clean terms.
  des <- build_me_design(tt, mdata, var1, var2, fake)
  brms_terms <- unname(name_map[des$term_names])
  if (anyNA(brms_terms) || !all(brms_terms %in% colnames(b)))
    stop("Could not match the nonlinear marginal-effect terms of '", var1,
         "' to the brms coefficients. Use brms-native marginal effects instead.")

  sim_mat <- b[, brms_terms, drop = FALSE]

  coef <- sim_coef_grad(sim_mat, des$G, fake, ci)
  diff_stats <- sim_diff_stats_grad(sim_mat, des$G, ci)

  list(coef, diff_stats$ci_diff, steps, diff_stats$ks_diff)
}
