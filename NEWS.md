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









