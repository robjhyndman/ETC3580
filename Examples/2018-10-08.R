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


## Assignment 2 revisited with a GAM
load("gss.RData")

gss %>%
  filter(year==2004,
         hgunlaw %in% c("AGREE", "DISAGREE")
  ) %>%
  mutate(
    guncontrol=(hgunlaw=="AGREE"),
    age = as.numeric(substr(age, 1, 2)),
    educ = as.numeric(educ),
    race = droplevels(race),
    owngun = fct_collapse(owngun,
               OTHER = c("REFUSED", "DK", "NA", "IAP")),
    owngun = recode_factor(owngun,
                           YES="YES", NO="NO",
                           IAP=NA_character_, REFUSED=NA_character_, DK=NA_character_, `NA`=NA_character_),
    attend = recode_factor(attend,
                           `MORE THN ONCE WK`= "> Weekly",
                           `EVERY WEEK` = "Weekly",
                           `NRLY EVERY WEEK` = "> Monthly",
                           `2-3X A MONTH` = "> Monthly",
                           `ONCE A MONTH` = "Monthly",
                           `SEVRL TIMES A YR` = "> Yearly",
                           `ONCE A YEAR` = "Yearly",
                           `LT ONCE A YEAR` = "< Yearly",
                           `NEVER` = "Never",
                           `DK,NA` = NA_character_),
    relig = fct_collapse(relig,
               OTHER = c("JEWISH", "OTHER", "BUDDHISM", "HINDUISM", "OTHER EASTERN",
                         "MOSLEM/ISLAM", "ORTHODOX-CHRISTIAN", "NATIVE AMERICAN",
                         "INTER-NONDENOMINATIONAL", "DK", "NA", "IAP")),
    relig = recode_factor(relig,
                          CHRISTIAN = "Christian",
                          PROTESTANT = "Christian",
                          CATHOLIC = "Catholic",
                          `ORTHODOX-CHRISTIAN` = "Christian",
                          `INTER-NONDENOMINATIONAL` = "Christian",
                          JEWISH = "Jewish",
                          `MOSLEM/ISLAM` = "Muslim",
                          BUDDHISM = "Eastern",
                          HINDUISM = "Eastern",
                          `OTHER EASTERN` = "Eastern",
                          OTHER = "Other",
                          NONE = "None",
                          `NA` = NA_character_
    ),
    relig = droplevels(relig),
    partyid = fct_collapse(partyid,
                           OTHER = c("NA", "DK", "OTHER PARTY")),
    partyid = recode_factor(partyid,
                            `STRONG DEMOCRAT` = "S Democrat",
                            `NOT STR DEMOCRAT` = "Democrat",
                            `IND,NEAR DEM` = "I Democrat",
                            `INDEPENDENT` = "Independent",
                            `IND,NEAR REP` = "I Republican",
                            `NOT STR REPUBLICAN` = "Republican",
                            `STRONG REPUBLICAN` = "S Republican",
                            `OTHER PARTY`  = "Other",
                            `DK` = NA_character_,
                            `NA` = NA_character_)
  ) %>%  
  select(guncontrol, age, sex, race, educ, relig, partyid, born, owngun, attend) ->
  guns

# Linear main effects model with sex*race interaction
fit1 <- gam(guncontrol ~ sex*race + age + educ + relig + born +
             owngun + partyid,
           family = binomial, data = guns, select=TRUE)
summary(fit1)

# Additive main effects model with sex*race interaction
fit2 <- gam(guncontrol ~ sex*race + s(age) + s(educ) + relig + born +
             owngun + partyid,
           family = binomial, data = guns, select=TRUE)
summary(fit2)
visreg(fit2, "age", gg=TRUE)
visreg(fit2, "educ", gg=TRUE)

# Can make education linear
fit3 <- gam(guncontrol ~ sex*race + s(age) + educ + relig + born +
              owngun + partyid,
            family = binomial, data = guns, select=TRUE)
summary(fit3)

anova(fit2, fit3, test="F")
anova(fit1, fit3, test="F")

visreg(fit3, "age", gg=TRUE)
visreg(fit3, "educ", gg=TRUE)
visreg(fit3, "relig", gg=TRUE)
visreg(fit3, "born", gg=TRUE)
visreg(fit3, "owngun", gg=TRUE)
visreg(fit3, "partyid", gg=TRUE)
visreg2d(fit3, x="sex", y="race")
