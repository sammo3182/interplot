## Submission summary

This is a feature release (1.2.0) of interplot. It adds:

* Three-way interaction support via a new `var3` argument, for `lm`, `glm`,
  mixed-effects (`lmer`/`glmer`), multiply-imputed, and Bayesian (`brms`) models.
* Nonlinear conditional effects: when the variable of interest interacts with a
  polynomial or spline transformation of the moderator, the conditional effect
  is plotted as the implied nonlinear curve.
* `bin_layer()`, a composable ggplot2 overlay implementing the Hainmueller,
  Mummolo, and Xu (2019) binning diagnostic.
* Johnson-Neyman intervals (`jn_interval()`, `jn_layer()`).
* A Bayesian method, `interplot.brmsfit()`.

It also moves `abind` and `arm` from Depends to Imports, fixes several bugs,
and adds a `testthat` test suite. The new `interplot()` arguments are optional
and default to the previous two-way behavior, so the change is backward
compatible.

## Test environments

* Windows 11, R Under development (unstable) (2026) -- local
* R CMD check run with `--as-cran`

## R CMD check results

0 errors | 0 warnings | 1 note

The single NOTE is from the CRAN incoming feasibility check and reports a
"possibly invalid URL" for an SSRN reference cited in the vignette
(`https://www.ssrn.com/abstract=2739221`). This is a transient TLS/network
failure on the local check machine (libcurl error 35, schannel handshake);
the URL is valid and resolves in a browser. The note also lists the
maintainer, which is expected for a new submission.

## Reverse dependencies

We are not aware of reverse dependencies affected by this release; the public
interface of `interplot()` is backward compatible.
