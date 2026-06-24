# Internal vectorized helpers for interplot (not exported)

#' Vectorized computation of conditional coefficients from simulation draws
#' @noRd
#' @keywords internal
sim_coef_vec <- function(sim_v1, sim_v12, fake, ci, multiplier = 1) {
  sim_matrix <- sim_v1 + multiplier * (sim_v12 %o% fake)
  probs <- c((1 - ci) / 2, 1 - (1 - ci) / 2)

  data.frame(
    fake = fake,
    coef1 = colMeans(sim_matrix),
    ub = apply(sim_matrix, 2, quantile, probs = probs[2]),
    lb = apply(sim_matrix, 2, quantile, probs = probs[1])
  )
}

#' General-gradient conditional effect from simulation draws
#'
#' Generalizes \code{sim_coef_vec} from the hardcoded linear basis
#' \code{[1, multiplier * z]} to an arbitrary gradient. \code{sim_mat} is the
#' (draws x terms) matrix of simulated coefficients for the terms that make up
#' the marginal effect of var1; \code{G} is the (n_fake x terms) basis matrix
#' giving each term's contribution to that effect across the var2 sequence.
#' The conditional effect for every draw is \code{sim_mat \%*\% t(G)}.
#' @noRd
#' @keywords internal
sim_coef_grad <- function(sim_mat, G, fake, ci) {
  sim_eff <- sim_mat %*% t(G) # draws x n_fake
  probs <- c((1 - ci) / 2, 1 - (1 - ci) / 2)

  data.frame(
    fake = fake,
    coef1 = colMeans(sim_eff),
    ub = apply(sim_eff, 2, quantile, probs = probs[2]),
    lb = apply(sim_eff, 2, quantile, probs = probs[1])
  )
}

#' ci_diff / ks_diff for the general-gradient effect (min vs max moderator)
#' @noRd
#' @keywords internal
sim_diff_stats_grad <- function(sim_mat, G, ci) {
  min_sim <- as.vector(sim_mat %*% G[1, ])
  max_sim <- as.vector(sim_mat %*% G[nrow(G), ])
  diff_sim <- max_sim - min_sim

  list(
    ci_diff = unname(stats::quantile(diff_sim, c((1 - ci) / 2, 1 - (1 - ci) / 2))),
    ks_diff = suppressWarnings(stats::ks.test(min_sim, max_sim))
  )
}

#' Simulate coefficient draws from N(mu, V) via Cholesky
#'
#' A drop-in alternative to \code{arm::sim} for the nonlinear path. \code{arm::sim}
#' silently drops spline/basis (matrix-valued) coefficient columns; sampling
#' directly from the coefficient vector \code{mu} and covariance \code{V} keeps
#' every term. \code{mu} is \code{coef(m)} for (g)lm and \code{fixef(m)} for
#' mixed models; \code{V} is the corresponding \code{vcov(m)}.
#' @noRd
#' @keywords internal
sim_betas_mvn <- function(mu, V, sims) {
  keep <- !is.na(mu) # drop aliased (rank-deficient) coefficients
  mu <- mu[keep]
  V <- as.matrix(V)[keep, keep, drop = FALSE]

  L <- chol(V) # V = t(L) %*% L
  Z <- matrix(stats::rnorm(sims * length(mu)), nrow = sims)
  draws <- Z %*% L + matrix(mu, nrow = sims, ncol = length(mu), byrow = TRUE)
  colnames(draws) <- names(mu)
  draws
}

#' Resolve the original model data (for rebuilding spline/polynomial bases)
#' @noRd
#' @keywords internal
me_model_data <- function(m, fallback = NULL) {
  data_full <- tryCatch(eval(stats::getCall(m)$data, environment(stats::formula(m))),
                        error = \(e) NULL)
  if (is.null(data_full)) data_full <- fallback
  data_full
}

#' Detect a nonlinear-in-moderator conditional effect
#'
#' Returns TRUE when the marginal effect of \code{var1} is a nonlinear function
#' of \code{var2}, i.e. \code{var1} interacts with a transformation of
#' \code{var2} (\code{I(var2^2)}, \code{poly(var2)}, \code{ns(var2)},
#' \code{bs(var2)}, ...) or with more than the single plain \code{var1:var2}
#' term. A plain two-way interaction returns FALSE so the fast legacy path runs.
#' \code{tt} is a \code{terms} object (use the fixed-effects-only terms for
#' mixed models).
#' @noRd
#' @keywords internal
detect_nonlinear <- function(tt, var1, var2) {
  factors <- attr(tt, "factors")
  if (is.null(factors) || !(var1 %in% rownames(factors))) return(FALSE)

  term_labels <- colnames(factors)
  v1_terms <- term_labels[factors[var1, ] > 0]

  involves_v2 <- vapply(v1_terms, \(tl) {
    vars <- tryCatch(all.vars(parse(text = tl)[[1]]), error = \(e) character(0))
    var2 %in% vars
  }, logical(1))

  v1v2_terms <- v1_terms[involves_v2]
  if (length(v1v2_terms) == 0) return(FALSE)

  plain <- v1v2_terms %in% c(paste0(var1, ":", var2), paste0(var2, ":", var1))
  any(!plain)
}

#' Build the (n_fake x terms) gradient basis for the var1 marginal effect
#'
#' Sets var1 = 1 and all other covariates to their null value (0 for numeric,
#' reference level for factors), sweeps var2 over \code{fake}, and reads the
#' design-matrix columns for the terms containing var1. Because the model is
#' linear in its coefficients, those columns are exactly the basis of the var1
#' marginal effect; \code{model.matrix} regenerates spline/polynomial bases at
#' the original knots via the terms' \code{predvars}. \code{tt} is the
#' response-deleted (fixed-effects) terms object; \code{data_full} is the
#' original model data.
#' @noRd
#' @keywords internal
build_me_design <- function(tt, data_full, var1, var2, fake) {
  needed <- all.vars(tt)
  miss <- setdiff(needed, names(data_full))
  if (length(miss))
    stop("Cannot reconstruct the design for nonlinear effects: variable(s) '",
         paste(miss, collapse = "', '"),
         "' not found in the model data. Refit the model with the data available.")

  ref <- lapply(needed, \(v) {
    x <- data_full[[v]]
    if (is.factor(x)) factor(levels(x)[1], levels = levels(x))
    else if (is.numeric(x)) 0
    else x[1]
  })
  names(ref) <- needed

  newdata <- as.data.frame(ref, stringsAsFactors = FALSE)[rep(1, length(fake)), , drop = FALSE]
  newdata[[var1]] <- 1
  newdata[[var2]] <- fake

  X <- stats::model.matrix(tt, newdata)
  asn <- attr(X, "assign")
  factors <- attr(tt, "factors")
  if (!(var1 %in% rownames(factors)))
    stop("var1 '", var1, "' is not a base term in the model.")

  v1_terms <- which(factors[var1, ] > 0)
  v1_cols <- which(asn %in% v1_terms)

  list(G = X[, v1_cols, drop = FALSE], term_names = colnames(X)[v1_cols])
}

#' Three-way / nonlinear handler for multiply-imputed models
#'
#' Shared by the \code{lmmi}/\code{glmmi}/\code{mlmmi}/\code{gmlmmi} methods.
#' Returns \code{NULL} when neither a three-way (\code{var3}) nor a nonlinear
#' moderator is requested (so the caller continues with its 2-way logic), or a
#' one-element list \code{list(value = ...)} wrapping the finished plot/data.
#' \code{draws_coef} are the imputation-pooled coefficient draws (for three-way);
#' nonlinear draws are rebuilt per imputation via \code{coef_fun}/\code{vcov_fun}
#' (because \code{arm::sim} drops spline columns) and pooled by stacking.
#' @noRd
#' @keywords internal
interplot_mi_extra <- function(m, m.list, draws_coef, frame, tt_fixed,
                               coef_fun, vcov_fun, df_resid,
                               var1, var2, var3, var3_vals, facet,
                               factor_v1, factor_v2, predPro,
                               plot, steps, ci, adjCI, hist, var2_dt, point,
                               sims, xmin, xmax, ercolor, esize, ralpha, rfill,
                               stats_cp, txt_caption, orig_var1, orig_var2, ...) {

  is_3way <- !is.null(var3)
  is_nonlinear <- !is_3way && !factor_v1 && !factor_v2 && predPro == FALSE &&
    var1 != var2 && detect_nonlinear(tt_fixed, var1, var2)
  if (!is_3way && !is_nonlinear) return(NULL)

  if (is_3way) {
    if (isTRUE(predPro))
      stop("Predicted probabilities are not supported for three-way interactions.")
    if (var1 == var2 || var1 == var3 || var2 == var3)
      stop("var1, var2, and var3 must be three distinct variables.")
    ls <- extract_coef_3way(
      draws = draws_coef, frame = frame, var1 = var1, var2 = var2, var3 = var3,
      var3_vals = var3_vals, ci = ci, adjCI = adjCI, df_resid = df_resid,
      steps = steps, xmin = xmin, xmax = xmax
    )
  } else {
    # Pool nonlinear draws across imputations by stacking N(coef, vcov) samples.
    draws <- do.call(rbind, map(m.list, \(i) sim_betas_mvn(coef_fun(i), as.matrix(vcov_fun(i)), sims)))
    ls <- extract_coef_nonlinear(
      tt = stats::delete.response(tt_fixed),
      data_full = me_model_data(m, fallback = frame),
      draws = draws, var1 = var1, var2 = var2, ci = ci, adjCI = adjCI,
      df_resid = df_resid, steps = steps, xmin = xmin, xmax = xmax
    )
  }

  coef_df <- ls[[1]]
  ci_diff <- ls[[2]]
  steps   <- ls[[3]]
  ks_diff <- ls[[4]]

  if (plot == FALSE) {
    names(coef_df)[1:4] <- c(var1, "coef", "ub", "lb")
    return(list(value = list(df_coef = coef_df, stats_ci = ci_diff)))
  }

  if (hist == TRUE && all(is.na(var2_dt))) var2_dt <- frame[[var2]]

  if (is_3way && stats_cp == "ci") {
    ci_note <- map2(ci_diff, levels(coef_df$value), \(aCI, aLevel) {
      paste0(aLevel, " CI(Max - Min): [",
             round(aCI[1], digits = 3), ", ",
             round(aCI[2], digits = 3), "]")
    }) |> list_c() |> paste(collapse = "\n")
    txt_caption <- paste0(ci_note, txt_caption)
  }

  overlay <- is_3way && !facet
  aPlot <- interplot.plot(
    m = coef_df, var1 = orig_var1, var2 = orig_var2, hist = hist, steps = steps,
    var2_dt = var2_dt, point = point, ercolor = ercolor, esize = esize,
    ralpha = ralpha, rfill = rfill, ci_diff = ci_diff, ks_diff = ks_diff,
    stats_cp = stats_cp, txt_caption = txt_caption, overlay = overlay, ...
  )
  if (is_3way && facet) {
    aPlot <- aPlot + ggplot2::facet_wrap(~ value)
  } else if (is_3way && !facet) {
    aPlot <- aPlot + ggplot2::labs(colour = var3, fill = var3)
  }

  list(value = aPlot)
}

#' Resolve an interaction coefficient name among the orderings R may emit
#'
#' Given the term components (e.g. c("wt", "cyl") or c("wt", "cyl", "hp")),
#' returns the first ":"-joined permutation that matches a coefficient name,
#' or NA if none match. R orders interaction terms by formula position, which
#' need not match the order var1/var2/var3 are passed to interplot.
#' @noRd
#' @keywords internal
resolve_term <- function(parts, coef_names) {
  perms <- if (length(parts) == 2) {
    list(parts, rev(parts))
  } else if (length(parts) == 3) {
    list(
      parts[c(1, 2, 3)], parts[c(1, 3, 2)], parts[c(2, 1, 3)],
      parts[c(2, 3, 1)], parts[c(3, 1, 2)], parts[c(3, 2, 1)]
    )
  } else {
    list(parts)
  }
  cand <- vapply(perms, \(p) paste(p, collapse = ":"), character(1))
  hit <- cand[cand %in% coef_names]
  if (length(hit)) hit[1] else NA_character_
}

#' Compute ci_diff and ks_diff statistics from simulation draws
#' @noRd
#' @keywords internal
sim_diff_stats <- function(sim_v1, sim_v12, xmin, xmax, ci, multiplier = 1) {
  min_sim <- sim_v1 + multiplier * xmin * sim_v12
  max_sim <- sim_v1 + multiplier * xmax * sim_v12
  diff_sim <- max_sim - min_sim

  list(
    ci_diff = unname(stats::quantile(diff_sim, c((1 - ci) / 2, 1 - (1 - ci) / 2))),
    ks_diff = stats::ks.test(min_sim, max_sim)
  )
}
