if(getRversion() >= "2.15.1") utils::globalVariables(c("fake", "coef1", "lb", "ub"))

#' Add a Binning Diagnostic to an interplot
#'
#' Overlays within-bin estimates of the conditional effect of \code{var1} on an
#' \code{interplot} output, following the binning estimator of Hainmueller,
#' Mummolo, and Xu (2019). It provides a visual check of the linear-interaction-
#' effect (LIE) assumption: if the moderating relationship is truly linear, the
#' binned point estimates fall on the interplot line; systematic departures
#' signal a nonlinear conditional effect.
#'
#' @param m A model object of class \code{lm} or \code{glm} including the
#'   interaction of \code{var1} and \code{var2}.
#' @param var1 The name (as a string) of the variable whose conditional effect is plotted.
#' @param var2 The name (as a string) of the moderating variable.
#' @param ci A numeric value defining the confidence level. The default is 0.95.
#' @param bins The number of moderator bins (quantile groups). The default is 3
#'   (low / medium / high terciles).
#' @param point_color Color of the binned points and whiskers. Default \code{"#BD472A"}.
#' @param point_shape Plotting shape of the binned points. Default \code{18} (filled diamond).
#'
#' @details For each quantile bin of \code{var2}, the model is refitted on the
#'   observations in that bin with \code{var2} centered at the bin median. The
#'   coefficient on \code{var1} is then its marginal effect evaluated at the bin
#'   median, estimated from only that bin's data; this is algebraically the
#'   Hainmueller-Mummolo-Xu L-estimator. Each estimate is drawn as a
#'   dot-and-whisker at the bin median.
#'
#'   Bins with singular or failed fits are dropped with a warning.
#'
#' @return A list of \code{ggplot2} layers, to be added to an \code{interplot}
#'   plot with \code{+}.
#'
#' @examples
#' m <- lm(mpg ~ wt * cyl, data = mtcars)
#' interplot(m, "cyl", "wt") + bin_layer(m, "cyl", "wt")
#'
#' @source Hainmueller, Jens, Jonathan Mummolo, and Yiqing Xu. 2019. "How Much
#'   Should We Trust Estimates from Multiplicative Interaction Models? Simple
#'   Tools to Improve Empirical Practice." Political Analysis 27(2): 163--192.
#'
#' @importFrom stats quantile median coef vcov update df.residual qnorm qt
#' @importFrom purrr map list_rbind
#' @import ggplot2
#' @export
bin_layer <- function(m, var1, var2, ci = 0.95, bins = 3,
                      point_color = "#BD472A", point_shape = 18) {

  if (!inherits(m, c("lm", "glm")))
    stop("bin_layer currently supports lm and glm objects.")

  mdata <- m$model
  if (is.factor(mdata[[var1]]) || is.factor(mdata[[var2]]))
    stop("bin_layer requires continuous var1 and var2.")
  if (var1 == var2)
    stop("bin_layer is not applicable to quadratic terms (var1 == var2).")

  z <- mdata[[var2]]
  breaks <- unique(stats::quantile(z, probs = seq(0, 1, length.out = bins + 1), na.rm = TRUE))
  if (length(breaks) - 1 < 2)
    stop("var2 does not have enough distinct values to form ", bins, " bins.")
  bin_id <- cut(z, breaks = breaks, include.lowest = TRUE, labels = FALSE)

  use_z <- inherits(m, "glm")

  est <- map(sort(unique(bin_id[!is.na(bin_id)])), \(j) {
    sub <- mdata[!is.na(bin_id) & bin_id == j, , drop = FALSE]
    zmed <- stats::median(sub[[var2]], na.rm = TRUE)
    sub[[var2]] <- sub[[var2]] - zmed # center so coef on var1 is the ME at the bin median

    mb <- tryCatch(stats::update(m, data = sub), error = function(e) NULL)
    if (is.null(mb) || !var1 %in% names(stats::coef(mb))) return(NULL)

    eff <- stats::coef(mb)[[var1]]
    se <- sqrt(stats::vcov(mb)[var1, var1])
    crit <- if (use_z) stats::qnorm(1 - (1 - ci) / 2)
            else stats::qt(1 - (1 - ci) / 2, stats::df.residual(mb))

    data.frame(fake = zmed, coef1 = eff, lb = eff - crit * se, ub = eff + crit * se)
  }) |> list_rbind()

  if (is.null(est) || nrow(est) == 0)
    stop("Could not estimate any bins (singular within-bin fits).")
  if (nrow(est) < length(unique(bin_id[!is.na(bin_id)])))
    warning("Some bins were dropped due to singular or failed fits.")

  list(
    geom_errorbar(data = est, aes(x = fake, ymin = lb, ymax = ub),
                  width = 0, linewidth = 0.7, color = point_color, inherit.aes = FALSE),
    geom_point(data = est, aes(x = fake, y = coef1),
               size = 2.5, shape = point_shape, color = point_color, inherit.aes = FALSE)
  )
}
