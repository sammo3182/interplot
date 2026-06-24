# interplot 1.2.0

## New features

- **Three-way interactions.** `interplot()` now accepts a third variable via `var3`, plotting the conditional effect of `var1` across `var2` at several values (or levels) of `var3`. Fixing `var3 = c` reduces the three-way effect to the existing two-way problem with a shifted intercept and slope, so the same simulation engine, `stats_cp`, and `adjCI` machinery are reused. Continuous `var3` defaults to the mean and mean +/- 1 SD (Aiken and West 1991); factor `var3` uses all levels. `facet = TRUE` (default) draws faceted small multiples; `facet = FALSE` overlays the curves colored by `var3`. Supported for `lm`, `glm`, mixed-effects (`lmerMod`, `glmerMod`), multiply-imputed (`lmmi`, `glmmi`, `mlmmi`, `gmlmmi`), and Bayesian (`brmsfit`) models, with continuous `var1`/`var2`.
- **Nonlinear conditional effects.** When `var1` interacts with a transformation of `var2` -- a polynomial (`var1:I(var2^2)`), `poly()`, or a spline (`splines::ns()`, `bs()`) -- `interplot()` now plots the conditional effect as the correct nonlinear function of `var2` instead of forcing a straight line. The marginal effect at each moderator value is the model's own design-matrix gradient `g(var2)' beta`, evaluated from posterior/simulation draws so that `stats_cp` and `adjCI` continue to work. The existing hardcoded quadratic (`multiplier = 2`) case is a special instance of this gradient. Detected automatically; a plain two-way interaction is unchanged. Because `arm::sim()` silently drops spline (matrix-valued) coefficient columns, the nonlinear path samples coefficients directly from `N(coef, vcov)` (`sim_betas_mvn`). Supported for `lm`, `glm`, mixed-effects, multiply-imputed (pooled across imputations), and Bayesian (`brmsfit`) models.
- **Binning diagnostic.** `bin_layer()` adds the Hainmueller, Mummolo, and Xu (2019) binning estimator as a composable ggplot2 overlay: within-bin (tercile) estimates of the conditional effect of `var1`, drawn as dot-and-whiskers at the bin medians. Departures from the `interplot` line flag violations of the linear-interaction-effect assumption. Usage: `interplot(m, "var1", "var2") + bin_layer(m, "var1", "var2")`.

## Bug fixes

- `jn_layer()` now clips its non-significant shading to the observed range of `var2`. Previously, when a Johnson-Neyman bound fell far outside the data range, the shading rectangle stretched the plot's x-axis to that bound, leaving most of the panel empty.

## Tests

- Added a `testthat` (edition 3) suite covering three-way interactions, the nonlinear gradient engine (including a 1e-12 equivalence guard that the new engine reproduces the existing linear and quadratic outputs exactly), `bin_layer()`, the `jn_layer()` axis clipping, and two-way backward compatibility.

# interplot 1.1.0

## New features

- Added support for Bayesian models fitted with `brms::brm()` via a new `interplot.brmsfit()` method. The conditional coefficient is computed directly from the model's posterior draws (no `arm::sim()` re-simulation), so the reported bounds are genuine equal-tailed posterior credible intervals. Numeric and factor base terms, `plot = FALSE`, `stats_cp`, `hist`, and `predPro` (via `brms::posterior_epred()` for Bernoulli/binomial families) are all supported. `brms` is a `Suggests`-only dependency, so it is loaded lazily and is not required for the rest of the package.
- Added Johnson-Neyman interval computation via `jn_interval()`. Identifies the values of the moderating variable where the conditional effect crosses significance boundaries. Works with `lm`, `glm`, `lmerMod`, and `glmerMod` objects.
- Added `jn_layer()` for composable ggplot2 overlays: vertical lines at JN bounds with shaded non-significant regions. Usage: `interplot(m, "var1", "var2") + jn_layer(jn)`.

## Performance and code quality

- Vectorized simulation computations using matrix outer products (`%o%`, `colMeans`, `apply`) replacing element-wise for loops. Shared helpers in `utils.R` (`sim_coef_vec`, `sim_diff_stats`) eliminate duplication across all method files.
- Replaced `eval(parse(text = ...))` with direct `[[` indexing throughout for safety and clarity.
- Replaced deprecated `summarize_all(funs(...))` with `summarize(across(...))`.
- Replaced growing `rbind` loops with `purrr::map()` + `list_rbind()`.
- Modernized to R 4.1+ idioms: native pipe `|>`, lambda `\(x)` syntax, `inherits()` for class checking.

## JSS reviewer fixes

- Plots now include default axis labels ("Conditional Effect of var1" / var2 name) instead of blank axes (Issue #41).
- Moved `abind` and `arm` from `Depends` to `Imports` so they are no longer loaded and attached on `library(interplot)` (Issue #42).
- Passing a data.frame without the required columns (`fake`, `coef1`, `ub`, `lb`) now gives a clear error message instead of a cryptic failure (Issue #44).

## Bug fixes

- Fixed `ci_diff` and `ks_diff` undefined in factor branches of `interplot.lmmi`, `interplot.glmmi`, `interplot.mlmmi`, and `interplot.gmlmmi`. These variables were referenced in `interplot.plot()` calls but never computed in factor interaction paths.
- Fixed `test_cp` not assigned in the histogram + `stats_cp = "ci"` code path of `interplot.plot`.
- Fixed ggplot2 `size` deprecation warnings: replaced `size` with `linewidth` in `geom_rect`, `geom_errorbar`, and `geom_ribbon` calls.
- Fixed the default x-axis label in `predPro` plots: the x-axis sweeps `var1`, so it is now labeled with `var1` instead of `var2`.
- Fixed `glm`/`lm` dispatch order in `interplot()`: `glm` is now checked before `lm` since `glm` inherits from `lm`.
- Removed stale scratch file (`replicated_file_ks.R`) that would execute bare expressions on package load.

# interplot 1.0.0

In this version, the structure of the primary function is rewritten.
The function treats factor and numeric base terms with separate side functions leaving room for more distinguished actions on the models with the two types of terms.

## New features

- `stats_cp = "ci"` works for factor base-term models.
- Allow to output confidence intervals of the difference between the minimum and maximum values of the conditioning variable when `plot = FALSE`.
- `predPro` can be used on multilevel models.

## Bug fix

- Fixed `stats_cp = "ci"` and `stats_cp = "ks"` for `lmerMod`/`glmerMod` objects. The `ks_diff` statistic was computed inside helper functions but not returned, causing undefined-variable errors when `stats_cp` was set to `"ci"` or `"ks"`. Now both `ci_diff` and `ks_diff` are properly propagated from `extract_coef_numM`/`extract_coef_facM` to `interplot.plot`. The same fix is applied to the `lm`/`glm` path for consistency.
- Update `interplot.plot` for the latest version of `ggplot2`.
- Cleaned up NAMESPACE imports and `globalVariables` declarations.


# interplot 0.2.3

## New features

1. Adding new statistics for testing the statistical significance of the conditional effect (by `stats_cp`).
1. Adding an argument to modify the plot caption (by `txt_caption`).

## Bug fix

1. Adding example of models based on multiple imputations.
1. Modifying the vignette to include new functions.

# interplot 0.2.2
* interplot no longer sets the random seed; to ensure complete reproducibility, users must now set their own seeds using `set.seed()` before calling interplot functions.
* Adds facet_labs argument, an optional character vector of facet labels to be used when plotting an interaction with a factor variable.

# interplot 0.2.1
## Function Updates
* Showing the confidence intervals between the conditional effects at the minimum and maximum values of the conditioning variable.

## Error Fixed
* Avoiding the warning caused by the `class(m) == "polr"`.

# interplot 0.2.0
## Adding Argument
* Adding an argument to adjust CIs to control the false discovery rate.
* Adding an argument to produce conditional predicted probabilities at given values.

## Vignette Updates
* Adding a brief review of the methodology of interaction.
* Adding an example to show how to control for the false discovery rate.
* Adding an example to illustrate plotting conditional predicted propbabilities.

# interplot 0.1.5
## Adding Argument
* Adding an argument to adjust the CIs.

# interplot 0.1.4
## Error Fixed
* Fixing the error in plotting `lmer` projects.


# interplot 0.1.3
## Function Updates
* Take the `steps` argument back in case of special design requirement of the plot.
* Fixed an error in presenting the histogram on categorical conditioning variables.
* Improving the histogram presentation: all the bars for categorical variables are centered.

## Vignette Updates
Updated the vignette including instructions of how to change the aesthetics of the plot and how to use histogram function.

# interplot 0.1.2.1
## Vignette Updates
Updated the vignette including instructions of how to change the aesthetics of the plot and how to use histogram function.

# interplot 0.1.2.0
## Additional Function
1. The aesthetics can be modified through built-in arguments or the ggplot `geom_` functions.
2. A histogram can be superimposed into the plot.


# interplot 0.1.1.1
## Additional Function
Adding the function to plot interactions based on factor variables.
## Bug fix
Fit `ggplot2` 2.0.0
Fixed the quadratic error (#16)


# interplot 0.1.1.0
## Additional Function
Adding the function to plot interactions based on factor variables.
## Bug fix
Fit `ggplot2` 2.0.0


# interplot 0.1.0.2
## Bug fix
Fix the bug for nonlinear multilevel models with multiply imputed data (gmlmmi).


# interplot 0.1.0.1
## Bug fix
Fix the error to run mlm and mlmmi.









