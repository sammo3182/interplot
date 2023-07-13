# Vignette building

devtools::install(build_vignettes = TRUE)

# Spell checking

library(devtools)
library(roxygen2)

spell_check()
check_rhub()
