## Test environments
* OS X, R 3.2.1
* Win 8.1, R 3.2.1
* Ubuntu 12.04, R 3.1.3


## R CMD check results

### V 0.1.0.0
There were no ERRORs or WARNINGs. 

This is a resubmission with corrections based on the comments from CRAN maintainer Kurt Hornik <Kurt.Hornik@wu.ac.at>. 

1. Change the maintainer to be a single person
2. Correct the title and description field of the DESCRIPTION
3. Change the website URL in vignette


### V 0.1.0.1

There were no ERRORs or WARNINGs. 

From Kurt Hornik

> We see
> * checking R code for possible problems ... NOTE
> interplot.default: no visible global function definition for "quantile"
> interplot.glmerMod: no visible global function definition for
>   "quantile"
> interplot.glmmi: no visible global function definition for "quantile"
> interplot.gmlmmi: no visible global function definition for "quantile"
> interplot.lmerMod: no visible global function definition for "quantile"
> interplot.lmmi: no visible global function definition for "quantile"
> interplot.mlmmi: no visible global function definition for "quantile"

change the nameplace for it.

### v 0.1.0.2
There were no ERRORs or WARNINGs. 
R CMD check succeeded

### v 0.1.1.0
There were no ERRORs or WARNINGs. 
R CMD check succeeded

### v 0.1.1.1
There were no ERRORs or WARNINGs. 
R CMD check succeeded

### v 0.1.2.0
There were no ERRORs or WARNINGs. 
R CMD check succeeded

### v 0.1.2.1
There were no ERRORs or WARNINGs. 
R CMD check succeeded
