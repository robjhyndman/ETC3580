library(faraway)
library(tidyverse)
library(visreg)
library(broom)

## Proportion responses

## Mammal sleep
help(mammalsleep)
head(mammalsleep)

# Compute proportion of time dreaming
# Add in animal names.
as.tibble(mammalsleep) %>%
  mutate(pdr=dream/sleep,
         animal=rownames(faraway::mammalsleep)) ->
  mammalsleep
mammalsleep

GGally::ggpairs(mammalsleep[,1:11])

summary(mammalsleep)

## Most missing values in the response variable
## Let's remove missing values to make comparisons easier
mammalsleep <- na.omit(mammalsleep)
summary(mammalsleep)

## Method 1: logitNormal
# Compute empirical logits
# First check for zero proportions
mammalsleep %>% arrange(pdr) %>%
  select(dream, sleep, pdr, animal)
# Don't echidna's ever dream??
# Let's just drop them.
msleep <- mammalsleep %>%
  filter(pdr > 0) %>%
  mutate(elogit = log(pdr/(1-pdr)))

msleep %>%
  ggplot(aes(x=body, y=elogit)) +
  geom_point() + scale_x_log10()
msleep %>%
  ggplot(aes(x=brain, y=elogit)) +
  geom_point() + scale_x_log10()
msleep %>%
  ggplot(aes(x=lifespan, y=elogit)) +
  geom_point() + scale_x_log10()
msleep %>%
  ggplot(aes(x=gestation, y=elogit)) +
  geom_point() + scale_x_log10()
msleep %>%
  ggplot(aes(x=predation, y=elogit)) +
  geom_point()
msleep %>%
  ggplot(aes(x=exposure, y=elogit)) +
  geom_point()
msleep %>%
  ggplot(aes(x=danger, y=elogit)) +
  geom_point()

mod1 <- lm(elogit ~ log(body)+ log(brain) + log(lifespan) + 
             log(gestation) + predation + exposure + danger,
            data=msleep)
summary(mod1)
drop1(mod1)

mod1 <- step(mod1, trace=FALSE)
summary(mod1)
visreg(mod1)

aug1 <- augment(mod1, type.predict = 'response') %>%
  mutate(
    pred1 = exp(.fitted)/(1+exp(.fitted)),
    animal = msleep$animal)

aug1 %>% 
  ggplot(aes(x=.hat,y=.cooksd)) + geom_point()
aug1 %>% 
  ggplot(aes(sample=.resid)) + geom_qq()

## Method 2: QuasiBinomial

mod2 <- glm(pdr ~ log(body)+ log(brain) + log(lifespan) +
              log(gestation) + predation + exposure + danger,
            family=quasibinomial, mammalsleep)
summary(mod2)
drop1(mod2,test="F")
# No AIC so we need to use F-tests. Drop one var at a time
# Crude variable selection but no other option for quasibinomial
glm(pdr ~ log(body) + log(lifespan) + danger,
    family=quasibinomial, mammalsleep) %>% drop1(test="F")

mod2 <- glm(pdr ~ log(body) + log(lifespan) + danger,
            family=quasibinomial, mammalsleep)
summary(mod2)
visreg(mod2)

aug2 <- augment(mod2, type.predict = 'response') %>%
  mutate(
    pred2 = .fitted,
    animal = mammalsleep$animal)

aug2 %>%
  ggplot(aes(x=.hat,y=.cooksd)) + geom_point()
aug2 %>% 
  ggplot(aes(x=.resid)) + geom_dotplot()

## Method 3: beta regression

library(mgcv)
mod3 <- gam(pdr ~ log(body) + log(lifespan) + danger,
            family=betar(), mammalsleep)
summary(mod3)
visreg(mod3)

aug3 <- augment(mod3, type.predict = 'response') %>%
  mutate(pred3 = .fitted,
         animal = mammalsleep$animal)

aug3 %>%
  ggplot(aes(x=.hat,y=.cooksd)) + geom_point()
aug3 %>% 
  ggplot(aes(x=.resid)) + geom_dotplot()

# Compare predictions
mammalsleep %>%
  left_join(aug1, by="animal") %>%
  left_join(aug2, by="animal") %>%
  left_join(aug3, by="animal") %>%
  ggplot(aes(x=pdr)) +
    geom_point(aes(y=pred1, col="LogitNormal")) +
    geom_point(aes(y=pred2, col="QuasiBinomial")) +
    geom_point(aes(y=pred3, col="Beta")) +
    geom_abline(intercept=0, slope=1)

