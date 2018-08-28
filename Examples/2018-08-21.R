## POISSON

library(faraway)
library(tidyverse)
library(visreg)
library(broom)

## Medical demand

library(AER)
?NMES1988
data(NMES1988)
summary(NMES1988)

NMES1988 <- NMES1988 %>%
  as_tibble() %>%
  mutate(region = relevel(region,"other"))

GGally::ggpairs(NMES1988[,c(6:8,13,15,18,1)])

NMES1988 %>% count(visits) 
NMES1988 %>% 
  ggplot(aes(x=visits)) + 
    geom_histogram(binwidth = 1, boundary=0)

# Looks like a lot of zeros!

ggplot(NMES1988, aes(x=chronic, y=visits)) +
  geom_point()
ggplot(NMES1988, aes(x=health, y=visits)) +
  geom_boxplot()
ggplot(NMES1988, aes(x=gender, y=visits)) +
  geom_boxplot()
ggplot(NMES1988, aes(x=insurance, y=visits)) +
  geom_boxplot()
ggplot(NMES1988, aes(x=factor(chronic), y=visits)) +
  geom_boxplot()
ggplot(NMES1988, aes(x=factor(hospital), y=visits)) +
  geom_boxplot()
ggplot(NMES1988, aes(x=school, y=visits)) +
  geom_point()

mod1 <- glm(visits ~ hospital + health + chronic + gender + school + insurance,
            family=poisson, data=NMES1988)
summary(mod1)
pchisq(deviance(mod1), df.residual(mod1), lower.tail=FALSE)

mod2 <- glm(visits ~ hospital + health + chronic + gender + school + insurance,
            family=quasipoisson, data=NMES1988)
summary(mod2)
drop1(mod2, test="F")
visreg(mod2)
augment(mod2) %>%
  ggplot(aes(x=.fitted, y=.resid)) +
  geom_point()

mod3 <- MASS::glm.nb(visits ~ hospital + health + chronic + gender + school + insurance,
            data=NMES1988)
summary(mod3)
pchisq(deviance(mod3), df.residual(mod3), lower.tail=FALSE)
drop1(mod3, test="Chisq")
visreg(mod3)
augment(mod3) %>%
  ggplot(aes(x=.fitted, y=.resid)) +
    geom_point()

library(pscl)
mod4 <- hurdle(
  visits ~ hospital + health + chronic + gender + school + insurance,
  data=NMES1988, dist='negbin')
summary(mod4)

mod5 <- hurdle(
  visits ~ hospital + health + chronic + gender + school + insurance |
          hospital + chronic + gender + school + insurance,
  data=NMES1988, dist='negbin')
lrtest(mod4, mod5)

mod6 <- zeroinfl(
  visits ~ hospital + health + chronic + gender + school + insurance,
  data=NMES1988, dist='negbin')
summary(mod6)

mod7 <- zeroinfl(
  visits ~ hospital + health + chronic + gender + school + insurance |
    hospital + chronic + gender + school + insurance,
  data=NMES1988, dist='negbin')
summary(mod7)
lrtest(mod6, mod7)

# The best model (smallest AIC) is mod5
AIC(mod1)
AIC(mod2)
AIC(mod3)
AIC(mod4)
AIC(mod5)
AIC(mod6)
AIC(mod7)

## Zero-inflated Poisson models

library(pscl)
help(bioChemists)
head(bioChemists)
summary(bioChemists)

modp <- glm(art ~ fem + mar + kid5 + phd + ment, data=bioChemists, family=poisson)
summary(modp)
pchisq(deviance(modp), df.residual(modp), lower.tail=FALSE)
# Does not fit well

# Observed vs predicted counts
bioChemists %>% 
  count(art) %>% 
  complete(art = 0:19, fill = list(n=0)) %>% 
  mutate(
    pred = colSums(predprob(modp)),
    e = n - pred
  )

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

newman <- tibble(fem="Men",mar="Single",kid5=0,ment=6)
predict(modz2, newdata=newman, type="prob")
predict(modz2, newdata=newman, type="zero")

