library(faraway)
library(mgcv)
library(visreg)
library(tidyverse)
library(broom)

## GENERALIZED ADDITIVE MODEL

# Dispersed Poisson model:
gammgcv <- gam(O3 ~ s(vh) + s(wind) + s(humidity) + s(temp) +
                 s(dpg) + s(vis) + s(doy) + s(ibh) + s(ibt),
               family=poisson,  data=ozone, select=TRUE)
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

augment(gammgcv) %>%
  ggplot(aes(x=.fitted, y=.resid)) +
    geom_point()



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

anova(fit2, fit3, test="Chisq")
anova(fit1, fit3, test="Chisq")

visreg(fit3, "age", gg=TRUE)
visreg(fit3, "educ", gg=TRUE)
visreg(fit3, "relig", gg=TRUE)
visreg(fit3, "born", gg=TRUE)
visreg(fit3, "owngun", gg=TRUE)
visreg(fit3, "partyid", gg=TRUE)
visreg2d(fit3, x="sex", y="race")

# Can make age and education interact
fit4 <- gam(guncontrol ~ sex*race + te(age, educ) + relig + born +
              owngun + partyid,
            family = binomial, data = guns, select=TRUE)
summary(fit4)
anova(fit4, fit3, test="Chisq")
visreg2d(fit4, "age", "educ", scale='response')

