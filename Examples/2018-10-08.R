library(faraway)
library(mgcv)
library(visreg)
library(tidyverse)
library(broom)

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

visreg2d(ammgcv, x="temp",y="ibh")
visreg2d(amint, x="temp",y="ibh", plot.type='persp')
visreg2d(ammgcv, x="temp",y="ibh", plot.type='rgl')

# Residual diagnostics
augment(ammgcv) %>%
  ggplot() +
    geom_point(aes(x=.fitted, y=.resid))
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
# select=TRUE means that terms may be dropped

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

augment(amred) %>%
  ggplot() +
  geom_point(aes(x=.fitted, y=.resid))

## Log with variable selection
ozone2 <- mutate(ozone,
                 logO3 = log(O3))
amred2 <- gam(logO3 ~ s(vh) + s(wind) + s(humidity) + s(temp) +
               s(dpg) + s(vis) + s(doy) + s(ibh) + s(ibt),
             data=ozone2, select=TRUE)
# select=TRUE means that terms may be dropped

summary(amred2)
augment(amred2) %>%
  ggplot() +
  geom_point(aes(x=.fitted, y=.resid))

visreg(amred2, "vh", gg=TRUE)
visreg(amred2, "wind", gg=TRUE)
visreg(amred2, "humidity", gg=TRUE)
visreg(amred2, "temp", gg=TRUE)
visreg(amred2, "dpg", gg=TRUE)
visreg(amred2, "vis", gg=TRUE)
visreg(amred2, "doy", gg=TRUE)
visreg(amred2, "ibh", gg=TRUE)
visreg(amred2, "ibt", gg=TRUE)


