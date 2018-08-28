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
GGally::ggpairs(gala[,1:7])

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
visreg(modp2)
summary(modp2)
## Note residual deviance is huge compared to df. (Should be about the same)
pchisq(deviance(modp2), df.residual(modp2), lower.tail=FALSE)

## Let's try a dispersed Poisson model
modd <- glm(Species ~ sqrt(Area) + sqrt(Elevation) + sqrt(Nearest) +
              sqrt(Scruz) + sqrt(Adjacent),
            family=quasipoisson, gala)
summary(modd)
# Check out the dispersion parameter -- a whopping 30!
drop1(modd,test="F")
# Only Elevation and Adjacent variables appear useful.
visreg(modd)

augment(modd) %>%
  ggplot(aes(x=.fitted, y=.resid)) +
  geom_point()

## Let's try a negative binomial model for the same data.
modnb <- MASS::glm.nb(Species ~ sqrt(Area) + sqrt(Elevation) + sqrt(Nearest) +
             sqrt(Scruz) + sqrt(Adjacent),
           gala)
summary(modnb)
pchisq(deviance(modnb), df.residual(modnb), lower.tail=FALSE)
drop1(modnb, test="Chisq")

visreg(modnb)
augment(modnb) %>%
  ggplot(aes(x=.fitted, y=.resid)) +
  geom_point()
