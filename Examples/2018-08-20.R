## POISSON

library(faraway)
library(tidyverse)
library(visreg)
library(broom)

# Galapagos

?gala
gala <- faraway::gala %>%
  as_tibble() %>%
  mutate(Island = rownames(faraway::gala))
gala

# Some pairwise scaterplots
GGally::ggpairs(gala)

# Fit regular linear model
modl <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent,
           gala)
augment(modl) %>%
  ggplot(aes(x=.fitted, y=.resid)) +
    geom_point()
## Variance increases with mean -- need to use Poisson

modp <- glm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent,
            family=poisson, gala)
summary(modp)
augment(modp) %>%
  ggplot(aes(x=.fitted, y=.resid)) +
  geom_point()

visreg(modp)

## Could transform some of these predictors. Many contain zeros
summary(gala)
modp2 <- glm(Species ~ sqrt(Area) + sqrt(Elevation) + sqrt(Nearest) +
               sqrt(Scruz) + sqrt(Adjacent),
            family=poisson, gala)
## Note residual deviance is huge compared to df. (Should be about the same)
1-pchisq(deviance(modp2), df.residual(modp2))

## Let's try a dispersed Poisson model
modd <- glm(Species ~ sqrt(Area) + sqrt(Elevation) + sqrt(Nearest) +
              sqrt(Scruz) + sqrt(Adjacent),
            family=quasipoisson, gala)
summary(modd)
# Check out the dispersion parameter -- a whopping 30!
drop1(modd,test="F")
# Only Elevation and Adjacent variables appear useful.
visreg(modp2)

augment(modp2) %>%
  ggplot(aes(x=.fitted, y=.resid)) +
  geom_point()

## Let's try a negative binomial model for the same data.
modnb <- MASS::glm.nb(Species ~ sqrt(Area) + sqrt(Elevation) + sqrt(Nearest) +
             sqrt(Scruz) + sqrt(Adjacent),
           gala)
summary(modnb)
drop1(modnb, test="Chisq")

visreg(modnb)
augment(modnb) %>%
  ggplot(aes(x=.fitted, y=.resid)) +
  geom_point()

autoplot(modnb)


## Zero-inflated Poisson models

library(pscl)
help(bioChemists)
head(bioChemists)
summary(bioChemists)

modp <- glm(art ~ fem + mar + kid5 + phd + ment, data=bioChemists, family=poisson)
summary(modp)
1-pchisq(deviance(modp), df.residual(modp))
# Does not fit well

bioChemists %>% count(art)
ocount <- bioChemists %>% count(art) %>% select(n) %>% as_vector()
pcount <- colSums(predprob(modp)[,1:15])
ocount - pcount
## Looks like too many zeros

modh <- hurdle(art ~ fem + mar + kid5 + phd + ment, data=bioChemists)
summary(modh)
modz <- zeroinfl(art ~ fem + mar + kid5 + phd + ment, data=bioChemists)
summary(modz)

ggplot() +
  geom_point(aes(x=fitted(modh), y=fitted(modz))) +
  xlab("Hurdle predictions") + ylab("ZIP predictions") +
  geom_abline(slope=1, intercept=0)

modz2 <- zeroinfl(art ~ fem + kid5 + ment | ment, data=bioChemists)
summary(modz2)

newman <- data.frame(fem="Men",mar="Single",kid5=0,ment=6)
predict(modz2, newdata=newman, type="prob")
predict(modz2, newdata=newman, type="zero")

