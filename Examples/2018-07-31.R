library(faraway)
library(tidyverse)
library(visreg)

#############################################
# gavote example
#############################################

gavote <- gavote %>%
  as_tibble() %>%
  rename(usage = rural) %>%
  mutate(
    undercount = (ballots - votes) / ballots,
    pergore = gore / votes,
    county = rownames(faraway::gavote)
  )

# best model with up to 2-way interactions
best2  <- lm(
    undercount ~ (pergore + perAA + equip + econ + usage + atlanta)^2,
    data=gavote) %>% 
  step(trace=FALSE)

summary(best2)

# Diagnostic plots for linear model
library(broom)
lmod_aug <- augment(best2) 

# Identify influential points 
lmod_aug %>% 
  ggplot(aes(x=.cooksd)) + 
    geom_dotplot() 
lmod_aug %>% 
  filter(.cooksd > 0.2) %>% 
  glimpse() 
 
lmod_aug %>% 
  ggplot(aes(x=.hat)) + 
  geom_dotplot() 
lmod_aug %>% 
  filter(.hat > 0.5) %>% 
  glimpse() 
 
 
## AIRQUALITY 
 
airquality <- airquality %>% 
  as_tibble() %>% 
  select(Solar.R, Wind, Temp, Ozone) 
 
best <- lm(Ozone ~ (Solar.R + Wind + Temp + I(Wind^2) + I(Temp^2))^2, 
                data = airquality) %>% 
  step(trace=FALSE) 
summary(best) 
 
aq_aug <- augment(best) 
dim(aq_aug) 
dim(airquality) 
# Due to missing values. 
 
# Re-fit the model with na.action=na.exclude 
best <- update(best, na.action=na.exclude) 
aq_aug <- augment(best, airquality) 
dim(aq_aug) 
dim(airquality) 
 
aq_aug <- augment(best, airquality)
 
# Check heteroscedasticity 
aq_aug %>% 
  ggplot(aes(x=.fitted, y=.resid)) + 
  geom_point() + geom_smooth() 
 
# Check linearity of relationships and homogeneity of groups 
aq_aug %>% 
  ggplot(aes(x=Solar.R, y=.resid)) + 
  geom_point() + geom_smooth() 
aq_aug %>% 
  ggplot(aes(x=Wind, y=.resid)) + 
  geom_point() + geom_smooth() 
aq_aug %>% 
  ggplot(aes(x=Temp, y=.resid)) + 
  geom_point() + geom_smooth() 
 
# Check residual normality 
aq_aug %>% 
  ggplot(aes(x=.resid)) + 
  geom_histogram(bins = nclass.FD(na.omit(aq_aug$.resid)), 
                 boundary=0) 
aq_aug %>% 
  ggplot(aes(sample=.resid)) + 
  geom_qq() 
 
# Identify influential points 
aq_aug %>% 
  ggplot(aes(x=.cooksd)) + 
  geom_dotplot() 
aq_aug %>% 
  filter(.cooksd > 0.25) %>% 
  glimpse() 
 
aq_aug %>% 
  ggplot(aes(x=.hat)) + 
  geom_dotplot() 
aq_aug %>% 
  filter(.hat > 0.5) %>% 
  glimpse() 

