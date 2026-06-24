#' Compute Johnson-Neyman Interval for Interaction Effects
#'
#' Identifies the values of the moderating variable at which the
#' conditional effect of the other variable transitions between
#' statistical significance and non-significance.
#'
#' @param m A model object (\code{lm}, \code{glm}, \code{lmerMod}, or \code{glmerMod}).
#' @param var1 The name (as a string) of the variable whose conditional effect is of interest.
#' @param var2 The name (as a string) of the moderating variable.
#' @param ci A numeric value defining the confidence level. The default is 0.95.
#'
#' @details The Johnson-Neyman (JN) technique finds the values of the moderating
#' variable (\code{var2}) at which the conditional effect of \code{var1} is
#' exactly at the boundary of statistical significance. This is computed
#' analytically from the regression coefficients and their variance-covariance
#' matrix.
#'
#' For linear mixed-effects models (\code{lmerMod}, \code{glmerMod}), a normal
#' approximation (z-distribution) is used instead of the t-distribution due
#' to the controversy over appropriate degrees of freedom.
#'
#' The function does not support factor variables or quadratic terms (\code{var1 == var2}).
#'
#' @return An object of class \code{jn_interval} containing:
#' \describe{
#'   \item{bounds}{All Johnson-Neyman bounds (may be 0, 1, or 2 values).}
#'   \item{bounds_in_range}{Bounds that fall within the observed data range of \code{var2}.}
#'   \item{var2_range}{The range of the moderating variable in the data.}
#'   \item{sig_pattern}{One of \code{"always"}, \code{"never"}, \code{"between"}, \code{"outside"}, \code{"below"}, or \code{"above"}, indicating where the effect is statistically significant.}
#'   \item{ns_regions}{List of \code{(xmin, xmax)} pairs marking non-significant regions.}
#' }
#'
#' @examples
#' m <- lm(mpg ~ wt * cyl, data = mtcars)
#' jn <- jn_interval(m, "cyl", "wt")
#' print(jn)
#'
#' # Add JN bounds to an interplot
#' interplot(m, "cyl", "wt") + jn_layer(jn)
#'
#' @importFrom stats qt coef vcov df.residual
#' @export
jn_interval <- function(m, var1, var2, ci = 0.95) {

  if (inherits(m, c("lmerMod", "glmerMod"))) {
    coefs <- lme4::fixef(m)
    vc <- as.matrix(stats::vcov(m))
    df_resid <- Inf
    model_data <- m@frame
  } else if (inherits(m, c("lm", "glm"))) {
    coefs <- stats::coef(m)
    vc <- stats::vcov(m)
    df_resid <- stats::df.residual(m)
    model_data <- m$model
  } else {
    stop("jn_interval supports lm, glm, lmerMod, and glmerMod objects.")
  }

  coef_names <- names(coefs)

  if (is.factor(model_data[[var1]]) || is.factor(model_data[[var2]]))
    stop("Johnson-Neyman intervals require continuous variables for both var1 and var2.")

  if (var1 == var2)
    stop("Johnson-Neyman intervals are not applicable to quadratic terms (var1 == var2).")

  var12 <- paste0(var2, ":", var1)
  if (!var12 %in% coef_names) var12 <- paste0(var1, ":", var2)
  if (!var12 %in% coef_names)
    stop("Model does not include the interaction of ", var1, " and ", var2, ".")

  b1 <- unname(coefs[var1])
  b3 <- unname(coefs[var12])
  v_b1 <- vc[var1, var1]
  v_b3 <- vc[var12, var12]
  cov_b1b3 <- vc[var1, var12]

  t_crit <- stats::qt(1 - (1 - ci) / 2, df_resid)

  a <- b3^2 - t_crit^2 * v_b3
  b_q <- 2 * (b1 * b3 - t_crit^2 * cov_b1b3)
  c_q <- b1^2 - t_crit^2 * v_b1
  disc <- b_q^2 - 4 * a * c_q

  if (disc < 0) {
    bounds <- numeric(0)
  } else if (abs(a) < .Machine$double.eps) {
    if (abs(b_q) < .Machine$double.eps) {
      bounds <- numeric(0)
    } else {
      bounds <- -c_q / b_q
    }
  } else {
    z1 <- (-b_q - sqrt(disc)) / (2 * a)
    z2 <- (-b_q + sqrt(disc)) / (2 * a)
    bounds <- sort(c(z1, z2))
  }

  var2_range <- range(model_data[[var2]], na.rm = TRUE)
  bounds_in_range <- bounds[bounds >= var2_range[1] & bounds <= var2_range[2]]

  is_sig <- function(z) {
    eff <- b1 + b3 * z
    se <- sqrt(v_b1 + z^2 * v_b3 + 2 * z * cov_b1b3)
    abs(eff / se) > t_crit
  }

  if (length(bounds) == 2) {
    sig_between <- is_sig(mean(bounds))
    sig_pattern <- if (sig_between) "between" else "outside"
    ns_regions <- if (sig_between) {
      list(c(-Inf, bounds[1]), c(bounds[2], Inf))
    } else {
      list(c(bounds[1], bounds[2]))
    }
  } else if (length(bounds) == 1) {
    sig_below <- is_sig(bounds[1] - 1)
    sig_pattern <- if (sig_below) "below" else "above"
    ns_regions <- if (sig_below) {
      list(c(bounds[1], Inf))
    } else {
      list(c(-Inf, bounds[1]))
    }
  } else {
    center_sig <- is_sig(mean(var2_range))
    sig_pattern <- if (center_sig) "always" else "never"
    ns_regions <- if (center_sig) list() else list(c(-Inf, Inf))
  }

  structure(
    list(
      bounds = bounds,
      bounds_in_range = bounds_in_range,
      var2_range = var2_range,
      var1 = var1,
      var2 = var2,
      ci = ci,
      b1 = b1,
      b3 = b3,
      sig_pattern = sig_pattern,
      ns_regions = ns_regions
    ),
    class = "jn_interval"
  )
}


#' @export
print.jn_interval <- function(x, ...) {
  cat("Johnson-Neyman Interval\n")
  cat("=======================\n")
  cat("Variable of interest:", x$var1, "\n")
  cat("Moderating variable: ", x$var2, "\n")
  cat("Confidence level:    ", x$ci * 100, "%\n\n")

  if (length(x$bounds) == 0) {
    if (x$sig_pattern == "always") {
      cat("The effect of", x$var1, "is significant across the entire range of", x$var2, ".\n")
    } else {
      cat("The effect of", x$var1, "is not significant at any value of", x$var2, ".\n")
    }
  } else {
    cat("JN bound(s):", paste(round(x$bounds, 3), collapse = ", "), "\n")

    if (length(x$bounds_in_range) > 0) {
      cat("Within data range [", round(x$var2_range[1], 3), ", ",
          round(x$var2_range[2], 3), "]: ",
          paste(round(x$bounds_in_range, 3), collapse = ", "), "\n", sep = "")
    } else {
      cat("No bounds within the observed data range [",
          round(x$var2_range[1], 3), ", ", round(x$var2_range[2], 3), "].\n", sep = "")
    }

    sig_desc <- switch(x$sig_pattern,
      "between" = paste0("The effect is significant when ", x$var2,
                         " is between ", round(x$bounds[1], 3),
                         " and ", round(x$bounds[2], 3), "."),
      "outside" = paste0("The effect is significant when ", x$var2,
                         " is below ", round(x$bounds[1], 3),
                         " or above ", round(x$bounds[2], 3), "."),
      "below"   = paste0("The effect is significant when ", x$var2,
                         " is below ", round(x$bounds[1], 3), "."),
      "above"   = paste0("The effect is significant when ", x$var2,
                         " is above ", round(x$bounds[1], 3), ".")
    )
    cat("\n", sig_desc, "\n", sep = "")
  }

  invisible(x)
}


#' Add Johnson-Neyman Bounds to an interplot
#'
#' Returns a list of \code{ggplot2} layers that overlay Johnson-Neyman significance
#' boundaries on an \code{interplot} output. Add to any interplot with \code{+}.
#'
#' @param jn A \code{jn_interval} object produced by \code{\link{jn_interval}}.
#' @param line_color Color of the boundary lines. Default \code{"red"}.
#' @param linetype Line type for boundaries. Default \code{"dashed"}.
#' @param shade_color Fill color for non-significant regions. Default \code{"grey80"}.
#' @param shade_alpha Transparency of shading. Default \code{0.15}.
#' @param label Logical; if \code{TRUE} (default), annotate the JN bound values.
#'
#' @return A list of \code{ggplot2} layers.
#'
#' @examples
#' m <- lm(mpg ~ wt * cyl, data = mtcars)
#' jn <- jn_interval(m, "cyl", "wt")
#' interplot(m, "cyl", "wt") + jn_layer(jn)
#'
#' @import ggplot2
#' @export
jn_layer <- function(jn, line_color = "red", linetype = "dashed",
                     shade_color = "grey80", shade_alpha = 0.15,
                     label = TRUE) {
  if (!inherits(jn, "jn_interval"))
    stop("jn must be a jn_interval object from jn_interval().")

  layers <- list()

  # Shade only the part of each non-significant region that falls within the
  # observed range of var2, so the overlay never widens the plot's x-axis
  # beyond the data (JN bounds can lie far outside the data range).
  rng <- jn$var2_range
  for (r in jn$ns_regions) {
    lo <- max(r[1], rng[1])
    hi <- min(r[2], rng[2])
    if (lo < hi) {
      layers <- c(layers, list(
        annotate("rect", xmin = lo, xmax = hi,
                 ymin = -Inf, ymax = Inf,
                 fill = shade_color, alpha = shade_alpha)
      ))
    }
  }

  if (length(jn$bounds_in_range) > 0) {
    layers <- c(layers, list(
      geom_vline(xintercept = jn$bounds_in_range,
                 color = line_color, linetype = linetype, linewidth = 0.7)
    ))

    if (label) {
      for (b in jn$bounds_in_range) {
        layers <- c(layers, list(
          annotate("text", x = b, y = Inf,
                   label = paste0("JN = ", round(b, 2)),
                   vjust = 1.5, hjust = -0.1, size = 3, color = line_color)
        ))
      }
    }
  }

  layers
}
