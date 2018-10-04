library(faraway)
library(mgcv)
library(visreg)
library(tidyverse)

# ADDITIVE MODELS

help(ozone)

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
# Ref.df is another estimate of equivalent df or some kind of upper bound?

visreg(ammgcv, "temp", gg=TRUE)
visreg(ammgcv, "ibh", gg=TRUE)
visreg(ammgcv, "ibt", gg=TRUE)

anova(ammgcv, olm, test="F")

# Test linearity of ibt
fit2 <- gam(O3 ~ s(temp) + s(ibh) + ibt, data=ozone)
anova(ammgcv, fit2, test="F")
# Marginally non-signifcant. Can possibly drop linearity
# But term is not actually significant at all (from summary)
# So could drop it altogether.

# Test for interaction between temp and ibh
# Using tensor product splines to allow for different scales
ammgcv <- gam(O3 ~ s(temp) + s(ibh), data=ozone)
amint <- gam(O3 ~ te(temp,ibh), data=ozone)

summary(ammgcv)
summary(amint)
anova(ammgcv, amint, test="F")
# Interaction not significant

visreg2d(amint, x="temp",y="ibh")
visreg2d(amint, x="temp",y="ibh", plot.type='persp')
visreg2d(amint, x="temp",y="ibh", plot.type='rgl')

# Residual diagnostics
ggplot() +
  geom_point(aes(x=fitted(ammgcv), y=residuals(ammgcv)))
# Some heteroskedasticity

augment(ammgcv) %>%
  ggplot(aes(x=.resid)) +
  geom_histogram()

augment(ammgcv) %>%
  ggplot(aes(sample=.resid)) +
  geom_qq()
# Very close to normal

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
