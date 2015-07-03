library(readr)
library(lme4)
library(arm)
library(dplyr)
library(ggplot2)

pew1.w <- read_csv("dev/pew1_w.csv")

t1m1 <- glmer(formula=meritocracy~ginicnty+income_i+ginicnty:income_i+income_cnty+black_cnty+
                  perc_bush04+pop_cnty+educ_i+age_i+gender_i+unemp_i+union_i+partyid_i+
                  ideo_i+attend_i+survid2006+survid2007+survid2009+(1+income_i|fips),
              data=pew1.w,family=binomial(link="logit"))
summary(t1m1)

# predicted probabilities with confidence intervals (see http://glmm.wikidot.com/faq under lme4)
newdat <- pew1.w %>% summarise_each(funs(mean))
newdat <- newdat[rep(1, 200), ]
newdat$ginicnty <- rep(seq(min(pew1.w$ginicnty, na.rm=T), max(pew1.w$ginicnty, na.rm=T), length.out = 100), times = 2) # Full observed range of ginicnty
newdat$income_i <- c(rep(quantile(pew1.w$income_i, .001), 100), rep(max(pew1.w$income_i), 100)) # One imputed value falls below theoretical range of variable, use actual range instead by taking 0.1 percentile value
newdat$fips <- NULL # mean FIPs meaningless (and causes error); not used anyway with re.form=NA
newdat$inc[1:100] <- "low"
newdat$inc[101:200] <- "high"

mm <- model.matrix(terms(t1m1), newdat)
newdat$pred <- predict(t1m1, newdat, re.form=NA, type="link")
pvar1 <- diag(mm %*% tcrossprod(vcov(t1m1), mm)) # FE variance only (no RE variance)
newdat <- data.frame(
    newdat,
    pp = invlogit(newdat$pred),
    pplo = invlogit(newdat$pred+qnorm(.025)*sqrt(pvar1)),
    pphi = invlogit(newdat$pred+qnorm(.975)*sqrt(pvar1))
)

t1m1.pp <- ggplot(newdat, aes(x=ginicnty, y=pp, colour=inc)) + geom_line() + 
    labs(x = "County Income Inequality", 
         y = "Probability of Rejecting Meritocracy") +
    geom_ribbon(aes(ymin = pplo, ymax = pphi, fill=inc, linetype=NA), 
                alpha = .25) +
    geom_text(aes(.5, .17, label = "Highest Income", colour="high"), size=4.5) +
    geom_text(aes(.25, .32, label = "Lowest Income", colour="low"), size=4.5) +
    scale_colour_grey(end=.6) + scale_fill_grey(end=.6) + 
    theme_bw() + theme(legend.position="none")
#ggsave(file="t1m1_pp.pdf", width=8, height=5.25)