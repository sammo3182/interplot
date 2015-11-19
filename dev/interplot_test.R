library(mi)
library(mitools)
library(Amelia)

p2005 <- read_sav("../Meritocracy_Rep/data/pew/merit/version_a/Dec05/Dec05c.sav")
p2005x <- data.frame(
    resp = p2005$resp,
    fips = as.numeric(p2005$qfips),
    state = floor(as.numeric(p2005$qfips)/1000),
    rej_merit = ifelse(p2005$q14k<=2, 0, ifelse(p2005$q14k>4, NA, 1)),
    income = ifelse(p2005$income<=9, p2005$income, NA), # 1 to 9
    educ = ifelse(p2005$educ<=7, p2005$educ, NA), # 1 to 7
    age = ifelse(p2005$age<99, p2005$age, NA),
    male = ifelse(p2005$sex==1, 1, 0),
    white = ifelse(p2005$race==1 & p2005$hisp!=1, 1, 0),
    union = ifelse(p2005$labor<=3, 1, ifelse(p2005$labor==4, 0, NA)),
    ideo = 6 - ifelse(p2005$ideo<=5, p2005$ideo, NA), # 1 to 5
    attend = 7 - ifelse(p2005$attend<=6, p2005$attend, NA) # 1 to 6
)

m1 <- glm(rej_merit ~ income*educ + age, data = p2005x, family = binomial(link = "logit"))
interplot(m1, "income", "educ")

p2005x_mi <- missing_data.frame(p2005x)
p2005x_mi <- change_type(p2005x_mi, y = "resp", to = "irrelevant")
p2005x_mi <- change_type(p2005x_mi, y = "fips", to = "irrelevant")
p2005x_mi <- change_type(p2005x_mi, y = "state", to = "irrelevant")
p2005x_mi <- change_type(p2005x_mi, y = "income", to = "ordered-categorical")
p2005x_mi <- change_type(p2005x_mi, y = "educ", to = "ordered-categorical")
p2005x_mi <- change_type(p2005x_mi, y = "attend", to = "ordered-categorical")

ptm <- proc.time()
p2005x_im <- mi(p2005x_mi, n.chains=8, n.iter=30, seed=324, max.minutes=60)
proc.time() - ptm

m2_mi <- pool(formula = rej_merit ~ as.numeric(income) * as.numeric(educ) + age,
              family=binomial(link="logit"),
              data = p2005x_im)
# class "pooled" should be implemented

p2005x_mi_list <- complete(p2005x_im, m = 10)
p2005x_mi_list2 <- p2005x_mi_list
for (i in 1:10) {
        p2005x_mi_list2[[i]]$income <- as.numeric(p2005x_mi_list[[i]]$income)
        p2005x_mi_list2[[i]]$educ <- as.numeric(p2005x_mi_list[[i]]$educ)
        p2005x_mi_list2[[i]]$attend <- as.numeric(p2005x_mi_list[[i]]$attend)
}
p2005x_mi_list3 <- imputationList(p2005x_mi_list2)


m2_mitools <- with(p2005x_mi_list3, 
                  glm(formula = rej_merit ~ income * educ + age,
                  family=binomial(link="logit")))
interplot(m2_mitools, "income", "educ")

m3_mitools <- with(p2005x_mi_list3,
                   glmer(formula = rej_merit~income * educ + age +
                          (1+income|state), family=binomial(link="logit")))
interplot(m3_mitools, "income", "educ")


a <- amelia(p2005x, m = 5, ords = 4) # lgstc doesn't seem to work
a2 <- imputationList(a$imputations)

m4_amelia <- with(a2, glm(formula = rej_merit ~ income * educ + age,
                   family=binomial(link="logit")))
interplot(a4, "income", "educ")