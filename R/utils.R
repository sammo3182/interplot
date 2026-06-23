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
