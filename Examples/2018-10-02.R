library(faraway)
library(KernSmooth)
library(MASS)
library(mgcv)
library(visreg)
library(splines)
library(tidyverse)
library(broom)

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

fit <- as.data.frame(locpoly(mcycle$times, mcycle$accel, drv=2, degree=2, bandwidth=3))
p3 <- ggplot(fit) +
  geom_line(aes(x=x,y=y), col='blue', data=fit) +
  ylab("f'(times)") + xlab("times")

gridExtra::grid.arrange(p1,p2,p3,ncol=1)


## Multivariate

lomod <- loess(sr ~ pop15 + ddpi, data=savings)
visreg2d(lomod, 'pop15', 'ddpi', plot.type='persp')
visreg2d(lomod, 'pop15', 'ddpi')

smod <- gam(sr ~ s(pop15, ddpi), data=savings)
visreg2d(smod, 'pop15', 'ddpi', plot.type='persp')
visreg2d(smod, 'pop15', 'ddpi')

smod2 <- gam(sr ~ pop15*ddpi, data=savings)
anova(smod2, smod, test="F")

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

