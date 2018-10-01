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

plot(fit, residuals=TRUE)

library(visreg)
visreg(fit)
visreg(fit, gg=TRUE)

# Test for linearity

fitlm <- gam(waiting ~ eruptions, data=faithful)
anova(fit, fitlm, test="F")


# ADDITIVE MODELS

help(faraway::ozone)

ozone %>% 
  select(temp, ibh, ibt, O3) %>%
  GGally::ggpairs(aes(alpha=.1))

olm <- gam(O3 ~ temp + ibh + ibt, data=ozone)
summary(olm)

visreg(olm, "temp", gg=TRUE) + ylim(-4,40)
visreg(olm, "ibh", gg=TRUE) + ylim(-4,40)
visreg(olm, "ibt", gg=TRUE) + ylim(-4,40)

ammgcv <- gam(O3 ~ s(temp) + s(ibh) + s(ibt), 
              data=ozone)
summary(ammgcv)

visreg(ammgcv, "temp", gg=TRUE)
visreg(ammgcv, "ibh", gg=TRUE)
visreg(ammgcv, "ibt", gg=TRUE)

anova(ammgcv, olm, test="F")

# Test linearity of ibt
fit2 <- gam(O3 ~ s(temp) + s(ibh) + ibt, data=ozone)
anova(ammgcv, fit2, test="F")

# Test for interaction between temp and ibh
amint <- gam(O3 ~ te(temp,ibh) + s(ibt), data=ozone)

summary(amint)
anova(ammgcv, amint, test="F")

visreg2d(amint, x="temp",y="ibh")
visreg2d(amint, x="temp",y="ibh", plot.type='persp')
visreg2d(amint, x="temp",y="ibh", plot.type='rgl')

# Residual diagnostics
ggplot() +
  geom_point(aes(x=fitted(ammgcv), y=residuals(ammgcv)))


augment(ammgcv) %>%
  ggplot(aes(x=.resid)) + 
  geom_histogram()
  
augment(ammgcv) %>%
  ggplot(aes(sample=.resid)) + 
  geom_qq()

# Predictions

predict(ammgcv, data.frame(temp=60,ibh=2000,ibt=100), se=TRUE)
predict(ammgcv, data.frame(temp=120,ibh=2000,ibt=100), se=TRUE)

## Full data set with variable selection

amred <- gam(O3 ~ s(vh) + s(wind) + s(humidity) + s(temp) + 
               s(dpg) + s(vis) + s(doy) + s(ibh) + s(ibt),
             data=ozone, select=TRUE)

summary(amred)
visreg(amred, "vh", gg=TRUE)
visreg(amred, "wind", gg=TRUE)
visreg(amred, "humidity", gg=TRUE)
visreg(amred, "temp", gg=TRUE)
visreg(amred, "dpg", gg=TRUE)
visreg(amred, "vis", gg=TRUE)
visreg(amred, "doy", gg=TRUE)
visreg(amred, "ibh", gg=TRUE)
visreg(amred, "ibt", gg=TRUE)



## GENERALIZED ADDITIVE MODEL

# Dispersed Poisson model:
gammgcv <- gam(O3 ~ s(vh) + s(wind) + s(humidity) + s(temp) + 
                 s(dpg) + s(vis) + s(doy) + s(ibh) + s(ibt),
               family=poisson,  scale=-1, data=ozone, select=TRUE)
summary(gammgcv)
visreg(gammgcv, "vh", gg=TRUE)
visreg(gammgcv, "wind", gg=TRUE)
visreg(gammgcv, "humidity", gg=TRUE)
visreg(gammgcv, "temp", gg=TRUE)
visreg(gammgcv, "dpg", gg=TRUE)
visreg(gammgcv, "vis", gg=TRUE)
visreg(gammgcv, "doy", gg=TRUE)
visreg(gammgcv, "ibh", gg=TRUE)
visreg(gammgcv, "ibt", gg=TRUE)


## Assignment 1 revisited with a GAM
library(tidyverse)
library(ggfortify)
library(forcats)
load("../Assignments/gss.RData")

# Example code
guns <- gss %>%
  filter(year==2004) %>%
  filter(hgunlaw %in% c("AGREE", "DISAGREE")) %>%
  mutate(
    guncontrol = (hgunlaw=="DISAGREE"),
    age = as.numeric(age),
    educ = as.numeric(educ),
    partyid = fct_collapse(partyid,
                           OTHER = c("NA", "DK", "OTHER PARTY")),
    owngun = fct_collapse(owngun,
                          OTHER = c("REFUSED", "DK", "NA", "IAP")),
    relig = fct_collapse(relig,
                         OTHER = c("JEWISH", "OTHER", "BUDDHISM", "HINDUISM", "OTHER EASTERN",
                                   "MOSLEM/ISLAM", "ORTHODOX-CHRISTIAN", "NATIVE AMERICAN",
                                   "INTER-NONDENOMINATIONAL", "DK", "NA", "IAP"))) %>%
  select(guncontrol, age, sex, race, educ, relig, partyid, born, owngun)

fit <- gam(guncontrol ~ sex*race + s(age) + s(educ) + relig + born +
             owngun + partyid,
           family = binomial, data = guns, select=TRUE)

summary(fit)
visreg(fit, "age", gg=TRUE)
visreg(fit, "educ", gg=TRUE)
visreg(fit, "relig", gg=TRUE)
visreg(fit, "born", gg=TRUE)
visreg(fit, "owngun", gg=TRUE)
visreg(fit, "partyid", gg=TRUE)
visreg2d(fit, x="sex", y="race")
