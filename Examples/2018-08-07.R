library(faraway)
library(tidyverse)
library(visreg)
library(broom)

#### BINARY REGRESSION

# Coronary Heart Disease
wcgs <- as_tibble(wcgs)
lmod <- glm(chd ~ height + cigs, family = binomial, wcgs)
summary(lmod)

## Visualize
visreg(lmod, "height", gg=TRUE) + theme_bw()
visreg(lmod, "cigs", gg=TRUE) + theme_bw()

## Residuals

wcgs_aug <- augment(lmod, 
                    type.resid='response', 
                    type.predict='link')
ggplot(wcgs_aug, 
       aes(x=.fitted, y=.resid, col=chd)) +
  geom_point(alpha=0.3) +
  xlab("Logit predictions") + ylab("Response residuals")

wcgs_aug <- augment(lmod, 
                    type.resid='pearson', 
                    type.predict='link')
ggplot(wcgs_aug, aes(x=.fitted, y=.resid, col=chd)) +
  geom_point(alpha=0.3) +
  xlab("Logit predictions") + ylab("Pearson residuals")

wcgs_aug <- augment(lmod, 
                    type.resid='deviance', 
                    type.predict='link')
ggplot(wcgs_aug, aes(x=.fitted, y=.resid, col=chd)) +
  geom_point(alpha=0.3) +
  xlab("Logit predictions") + ylab("Deviance residuals")

visreg(lmod, "cigs") + theme_bw()

# Don't expect residuals to be normally distributed
# or not to be patterned. 

wcgs_aug %>% 
  ggplot(aes(x=.cooksd, y=.hat)) +
  geom_point()

filter(wcgs_aug, .hat > 0.015) %>%
  select(height, cigs, chd)

wcgs_aug %>% 
  ggplot(aes(x=cigs, y=.hat)) +
  geom_point()

wcgs_aug %>% 
  ggplot(aes(x=height, y=.hat)) +
  geom_point()

wcgs_aug %>% 
  ggplot(aes(x=chd, y=.hat)) +
  geom_point()

## Deviance tests
summary(lmod)
anova(lmod, test="Chisq")
drop1(lmod, test="Chisq")

confint(lmod)

## Variable selection
### Add bmi
wcgs <- mutate(wcgs, bmi=703*weight/height^2)
### Fit big model with only main effects
lmod <- glm(chd ~ age + height + weight + bmi + sdp + 
                     dbp + chol + dibep + cigs + arcus,
            family=binomial, wcgs)
lmodr <- step(lmod, trace=FALSE)
summary(lmodr)
visreg(lmodr)

# Now add in interactions
lmod <- glm(chd ~ (age + bmi + sdp + 
              dbp + chol + dibep + cigs + arcus)^2,
            family=binomial, wcgs)
lmodr <- step(lmod, trace=FALSE)
summary(lmodr)
visreg2d(lmodr, 'bmi', 'chol')
visreg2d(lmodr, 'dbp', 'bmi')
visreg(lmodr, 'cigs', by='dibep', gg=TRUE) + theme_bw()

wcgs_aug <- augment(lmodr, 
                    type.resid='response', 
                    type.predict='link')

ggplot(wcgs_aug, aes(x=.fitted, y=.resid, col=chd)) +
  geom_point(alpha=0.3) +
  xlab("Logit predictions") + ylab("Deviance residuals")

wcgs_aug %>% 
  ggplot(aes(x=.cooksd, y=.hat)) +
  geom_point()

wcgs_aug %>% 
  filter(.hat > 0.3) %>%
  glimpse()

## Who supports Pinochet?

chile <- 
  as.tibble(carData::Chile) %>%
  mutate(vp = vote=="Y") %>%
  select( -population, -vote)

fit <- glm(vp ~ region + sex + age + education + income,
           family=binomial, data=chile)
summary(fit)
visreg(fit)

drop1(fit, test="Chisq")

chile1 <- augment(fit)

chile1 %>% 
  ggplot(aes(x=.cooksd, y=.hat)) +
  geom_point()

## Does probit make any difference?
fit2 <- glm(vp ~ region + sex + age + education + income,
            family=binomial(link=probit), data=chile)
summary(fit2)
anova(fit,fit2, test="Chisq")

