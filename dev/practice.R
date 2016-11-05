devtools::source_gist(9112634)
package(c("ggplot2", "interplot"))

m_cyl <- lmer(mpg ~ wt * hp + (wt | cyl), mtcars)
m_cyl <- lm(mpg ~ wt * cyl, mtcars)


m <- m_cyl
var1 <- "hp"
var2 <- "wt"
plot = TRUE
point = FALSE
sims = 5000
xmin = NA
xmax = NA
ercolor = NA
esize = .5
ralpha = .5
rfill = "grey70"
var2_dt <- NA


hist <- TRUE

var2_dt <- m@frame$hp

yrange<- c(coef$ub, coef$lb, var2_dt)

maxdiff<-(max(yrange)-min(yrange))

hist.out<-hist(var2_dt,breaks=80,plot=FALSE)
n.hist<-length(hist.out$mids)
dist<-hist.out$mids[2]-hist.out$mids[1]
hist.max<-max(hist.out$counts)

histX<-data.frame(ymin=rep(min(yrange)-maxdiff/5,n.hist),
                  ymax=hist.out$counts/hist.max*maxdiff/5+min(yrange)-maxdiff/5,
                  xmin=hist.out$mids-dist/2,
                  xmax=hist.out$mids+dist/2)



#interplot.plot(m = coef, var1 = "cyl", var2 = "wt") 

p1 <- ggplot()
p1 <- p1 + geom_rect(data=histX,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
                     colour="gray50",alpha=0,size=0.5)


p1 + geom_line(data = m, aes_string(x = "fake", y = "coef1")) + 
  geom_ribbon(data = m, aes_string(x = "fake",  ymin = "lb", ymax = "ub"), alpha = ralpha, color = ercolor, fill = rfill) + 
  ylab(NULL) + xlab(NULL)

