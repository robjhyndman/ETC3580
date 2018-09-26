library(faraway)
library(tidyverse)
library(KernSmooth)
library(MASS)
library(mgcv)
library(visreg)
library(splines)

## Inference

ggplot(mcycle, aes(x=times, y=accel)) +
  geom_point() +
  geom_smooth(span=0.2)

ggplot(mcycle, aes(x=times, y=accel)) +
  geom_point() +
  geom_smooth(method='gam',
              formula = y ~ s(x))

fit1 <- gam(accel ~ s(times), data=mcycle)
fit2 <- gam(accel ~ times, data=mcycle)
anova(fit1,fit2, test="F")


## Derivatives

fit <- as.data.frame(locpoly(mcycle$times, mcycle$accel, drv=0, degree=2, bandwidth=3))
p1 <- ggplot(mcycle, aes(x=times, y=accel)) +
  geom_point() +
  geom_line(aes(x=x,y=y), col='blue', data=fit) +
  ylab("f(times)") + xlab("times")

fit <- as.data.frame(locpoly(mcycle$times, mcycle$accel, drv=1, degree=2, bandwidth=3))
p2 <- ggplot(fit) +
  geom_line(aes(x=x,y=y), col='blue', data=fit) +
  ylab("f'(times)") + xlab("times")

gridExtra::grid.arrange(p1,p2,ncol=1)


## Multivariate

lomod <- loess(sr ~ pop15 + ddpi, data=savings)

xg <- seq(21,48,len=20)
yg <- seq(0,17,len=20)
zg <- expand.grid(pop15=xg,ddpi=yg)
par(mar=c(0,0,0,0))
persp(xg, yg, predict(lomod, zg), theta=-30,
      ticktype="detailed", col=heat.colors(500),
      xlab="pop15", ylab="ddpi", zlab="savings rate")

smod <- gam(sr ~ s(pop15, ddpi), data=savings)
vis.gam(smod, ticktype="detailed",theta=-30)
visreg2d(smod, 'pop15', 'ddpi')


## Penalized regression splines

fit <- lm(waiting ~ ns(eruptions, df=6),
          faithful)
ggplot(faithful) +
  geom_point(aes(x=eruptions,y=waiting)) +
  ggtitle("Old Faithful (Natural splines, 6 df)") +
  geom_line(aes(x=eruptions, y=fitted(fit)), col='blue')

knots <- quantile(faithful$eruptions,
                  prob=seq(.01,.99,by=.01))
Z <- matrix(0, nrow=nrow(faithful),
            ncol=length(knots))
for(k in seq_along(knots))
  Z[,k] <- pmax(0, (faithful$eruptions - knots[k])^3)

fit <- lm(waiting ~ poly(eruptions,3) + Z,
          data=faithful)
ggplot(faithful) +
  geom_point(aes(x=eruptions,y=waiting)) +
  ggtitle("Old Faithful regression splines") +
  geom_line(aes(x=eruptions, y=fitted(fit)), col='blue')

fit <- gam(waiting ~ s(eruptions, k=102, fx=TRUE),
           method="REML", data=faithful)

ggplot(faithful) +
  geom_point(aes(x=eruptions,y=waiting)) +
  ggtitle("Old Faithful penalized regression splines") +
  geom_line(aes(x=eruptions, y=fitted(fit)), col='blue')

fit <- gam(waiting ~ s(eruptions, k=102),
           method="REML", data=faithful)

ggplot(faithful) +
  geom_point(aes(x=eruptions,y=waiting)) +
  ggtitle("Old Faithful penalized regression splines") +
  geom_line(aes(x=eruptions, y=fitted(fit)), col='blue')

# Default mgcv approach

fit <- gam(waiting ~ s(eruptions),
           data=faithful)

ggplot(faithful) +
  geom_point(aes(x=eruptions,y=waiting)) +
  ggtitle("Old Faithful penalized regression splines") +
  geom_line(aes(x=eruptions, y=fitted(fit)),
            col='blue')

visreg(fit, gg=TRUE) + theme_bw()

# Test for linearity

fitlm <- gam(waiting ~ eruptions, data=faithful)
anova(fit, fitlm, test="F")
