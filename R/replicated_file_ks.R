m <- lm(mpg ~ wt * qsec, data = mtcars)

var1 = 'qsec'
var2 = 'wt'
var12 <- paste0(var2,  ":", var1)

m.sims <- arm::sim(m, 100)

xmin <- min(m$model[var2], na.rm = T)
xmax <- max(m$model[var2], na.rm = T)


min_sim <- m.sims@coef[, match(var1, names(m$coef))] + xmin * m.sims@coef[, match(var12, names(m$coef))] # simulation of the value at the minimum value of the conditioning variable
max_sim <- m.sims@coef[, match(var1, names(m$coef))] + xmax * m.sims@coef[, match(var12, names(m$coef))] # simulation of the value at the maximum value of the conditioning variable

ks.test(min_sim, max_sim)