library(faraway)
library(tidyverse)
library(KernSmooth)

# Fix exb data
exb <- as.tibble(exb)

# Local constant (kernel smoothing)

fit <- locpoly(faithful$eruptions, faithful$waiting,
               degree=0, bandwidth=0.3) %>% as.tibble
ggplot(faithful) +
  geom_point(aes(x=eruptions,y=waiting)) +
  geom_line(data=fit, aes(x=x,y=y), col='blue')

fit <- locpoly(exa$x, exa$y,
               degree=0, bandwidth=0.03) %>% as.tibble
ggplot(exa) +
  geom_point(aes(x=x,y=y)) +
  geom_line(data=fit, aes(x=x,y=y), col='blue') +
  geom_line(aes(x=x,y=m), col='red')

fit <- locpoly(exb$x, exb$y,
               degree=0, bandwidth=0.05) %>% as.tibble
ggplot(exb) +
  geom_point(aes(x=x,y=y)) +
  geom_line(data=fit, aes(x=x,y=y), col='blue') +
  geom_line(aes(x=x,y=m), col='red')


# Local linear

fit <- locpoly(faithful$eruptions, faithful$waiting,
               degree=1, bandwidth=0.3) %>% as.tibble
ggplot(faithful) +
  geom_point(aes(x=eruptions,y=waiting)) +
  geom_line(data=fit, aes(x=x,y=y), col='blue')

h <- dpill(faithful$eruptions,faithful$waiting)
fit <- locpoly(faithful$eruptions, faithful$waiting,
               degree=1, bandwidth=h) %>% as.tibble
ggplot(faithful) +
  geom_point(aes(x=eruptions,y=waiting)) +
  geom_line(data=fit, aes(x=x,y=y), col='blue')

h <- dpill(exa$x,exa$y)
fit <- locpoly(exa$x, exa$y,
               degree=1, bandwidth=h) %>% as.tibble
ggplot(exa) +
  geom_point(aes(x=x,y=y)) +
  geom_line(data=fit, aes(x=x,y=y), col='blue') +
  geom_line(aes(x=x,y=m), col='red')

h <- dpill(exb$x, exb$y)
fit <- locpoly(exb$x, exb$y,
               degree=1, bandwidth=h) %>% as.tibble
ggplot(exb) +
  geom_point(aes(x=x,y=y)) +
  geom_line(data=fit, aes(x=x,y=y), col='blue') +
  geom_line(aes(x=x,y=m), col='red')


# Local quadratic

fit <- locpoly(faithful$eruptions, faithful$waiting,
               degree=2, bandwidth=0.6) %>% as.tibble
ggplot(faithful) +
  geom_point(aes(x=eruptions,y=waiting)) +
  geom_line(data=fit, aes(x=x,y=y), col='blue')

fit <- locpoly(exa$x, exa$y,
               degree=2, bandwidth=0.05) %>% as.tibble
ggplot(exa) +
  geom_point(aes(x=x,y=y)) +
  geom_line(data=fit, aes(x=x,y=y), col='blue') +
  geom_line(aes(x=x,y=m), col='red')

fit <- locpoly(exb$x, exb$y,
               degree=2, bandwidth=0.06) %>% as.tibble
ggplot(exb) +
  geom_point(aes(x=x,y=y)) +
  geom_line(data=fit, aes(x=x,y=y), col='blue') +
  geom_line(aes(x=x,y=m), col='red')

## Loess

smr <- loess(waiting ~ eruptions, data=faithful)
ggplot(faithful) +
  geom_point(aes(x=eruptions,y=waiting)) +
  ggtitle("Old Faithful (Loess, span=0.75)") +
  geom_line(aes(x=eruptions, y=fitted(smr)), col='blue')

smr <- loess(y ~ x, data=exa, span=0.22)
ggplot(exa) +
  geom_point(aes(x=x,y=y)) +
  ggtitle("Example A (Loess, span=0.75)") +
  geom_line(aes(x=x, y=fitted(smr)), col='blue') +
  geom_line(aes(x=x,y=m), col='red')

smr <- loess(y ~ x, data=exb, family='symmetric')
ggplot(as.data.frame(exb)) +
  geom_point(aes(x=x,y=y)) +
  ggtitle("Example B (Robust Loess, span=0.75)") +
  geom_line(aes(x=x, y=fitted(smr)), col='blue') +
  geom_line(aes(x=x,y=m), col='red')


## geom_smooth (uses non-robust loess)

ggplot(faithful) +
  geom_point(aes(x=eruptions,y=waiting)) +
  geom_smooth(aes(x=eruptions,y=waiting), span=0.3) 

ggplot(exa) +
  geom_point(aes(x=x,y=y)) +
  geom_smooth(aes(x=x,y=y), method='loess',
              span=0.22)

ggplot(exb) +
  geom_point(aes(x=x,y=y)) +
  geom_smooth(aes(x=x,y=y), method='loess')


# Smoothing splines

lambda <- 0.001
smr <- smooth.spline(faithful$eruptions, faithful$waiting, lambda=lambda)
smr <- data.frame(x=smr$x,y=smr$y)
ggplot(faithful) +
  geom_point(aes(x=eruptions,y=waiting)) +
  ggtitle(paste("Old Faithful (Smoothing spline, lambda=",lambda, sep="")) +
  geom_line(data=smr, aes(x=x, y=y), col='blue')


smr <- smooth.spline(faithful$eruptions, faithful$waiting, cv=TRUE)
smr <- data.frame(x=smr$x,y=smr$y)
ggplot(faithful) +
  geom_point(aes(x=eruptions,y=waiting)) +
  ggtitle("Old Faithful (Smoothing spline, lambda chosen by CV)") +
  geom_line(data=smr, aes(x=x, y=y), col='blue')


smr <- smooth.spline(exa$x,exa$y, cv=TRUE)
smr <- data.frame(x=smr$x,y=smr$y)
ggplot(exa) +
  geom_point(aes(x=x,y=y)) +
  ggtitle("Example A (Smoothing spline, lambda chosen by CV)") +
  geom_line(data=smr, aes(x=x, y=y), col='blue') +
  geom_line(aes(x=x,y=m), col='red')


smr <- smooth.spline(exb$x,exb$y, cv=TRUE)
smr <- data.frame(x=smr$x,y=smr$y)
ggplot(exb) +
  geom_point(aes(x=x,y=y)) +
  ggtitle("Example B (Smoothing spline, lambda chosen by CV)") +
  geom_line(data=smr, aes(x=x, y=y), col='blue') +
  geom_line(aes(x=x,y=m), col='red')



## Regression splines

library(splines)
fit <- lm(waiting ~ ns(eruptions, df=6), faithful)
ggplot(faithful) +
  geom_point(aes(x=eruptions,y=waiting)) +
  ggtitle("Old Faithful (Natural splines, 6 df)") +
  geom_line(aes(x=eruptions, y=fitted(fit)), col='blue')

fit <- lm(y ~ ns(x, df=12), exa)
ggplot(exa) +
  geom_point(aes(x=x,y=y)) +
  ggtitle("Example A (Natural splines, 12 df)") +
  geom_line(aes(x=x, y=fitted(fit)), col='blue') +
  geom_line(aes(x=x,y=m), col='red')


fit <- lm(y ~ ns(x, df=3), exb)
ggplot(as.data.frame(exb)) +
  geom_point(aes(x=x,y=y)) +
  ggtitle("Example B (Natural splines, 3 df)") +
  geom_line(aes(x=x, y=fitted(fit)), col='blue') +
  geom_line(aes(x=x,y=m), col='red')

ggplot(exa) +
  geom_point(aes(x=x,y=y)) +
  geom_smooth(aes(x=x,y=y), method='gam',
              formula = y ~ s(x,k=12))


lomod <- loess(sr ~ pop15 + ddpi, data=savings)

xg <- seq(21,48,len=20)
yg <- seq(0,17,len=20)
zg <- expand.grid(pop15=xg,ddpi=yg)
par(mar=c(0,0,0,0))
persp(xg, yg, predict(lomod, zg), theta=-30, 
      ticktype="detailed", col=heat.colors(500),
      xlab="pop15", ylab="ddpi", zlab="savings rate")

smod <- mgcv::gam(sr ~ s(pop15, ddpi), data=savings)
mgcv::vis.gam(smod, ticktype="detailed",theta=-30)

