library(faraway)
library(tidyverse)
library(visreg)

## Why interactions matter

data <- tibble(
  x = runif(100, -1, 1),
  z = runif(100, -1, 1),
  y = 2*x*z + rnorm(100)
)

GGally::ggpairs(data)

fit <- lm(y ~ x + z, data=data)
summary(fit)
visreg(fit)

fit2 <- lm(y ~ x*z, data=data)
summary(fit2)
visreg2d(fit2, "x", "z")
visreg2d(fit2, "x", "z", plot.type="rgl")

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

# Fit big regression
lmod <- lm(
  undercount ~ pergore + perAA + equip + econ + usage + atlanta,
  data=gavote)

# Variable selection
best1 <- step(lmod)
summary(best1)
AIC(best1)

# include 2-way interactions
best2  <- lm(
    undercount ~ (pergore + perAA + equip + econ + usage + atlanta)^2,
    data=gavote) %>% 
  step(trace=FALSE)
# Best model:
summary(best2)
AIC(best2)
# Why are there missing values?
gavote %>% 
  count(econ, equip) %>%
  spread(equip, n)
## Too few observations for paper

visreg(best2, 'econ', by='equip', gg=TRUE) + theme_bw()
visreg(best2, 'perAA', by='equip', gg=TRUE) + theme_bw()
visreg(best2, 'perAA', by='usage', gg=TRUE) + theme_bw()
visreg2d(best2, 'perAA', 'pergore')
visreg2d(best2, 'perAA', 'pergore', plot.type='persp')
visreg2d(best2, 'perAA', 'pergore', plot.type='rgl')

# Diagnostic plots for linear model
library(broom)
lmod_aug <- augment(best2) %>%
  bind_cols(select(gavote, atlanta))

# Check heteroscedasticity
lmod_aug %>%
  ggplot(aes(x=.fitted, y=.resid)) +
    geom_point() + geom_smooth()

# Check linearity of relationships and homogeneity of groups
lmod_aug %>%
  gather(x, val, c("atlanta","econ","equip","perAA","pergore","usage")) %>%
  ggplot(aes(val, .resid)) +
    geom_point() +
    facet_wrap(~x, scales='free')

# Check residual normality
lmod_aug %>%
  ggplot(aes(x=.resid)) +
  geom_histogram(bins = nclass.FD(lmod_aug$.resid),
                 boundary=0)
lmod_aug %>%
  ggplot(aes(sample=.resid)) +
  geom_qq()

# Look at cases with large residuals
lmod_aug %>%
  filter(.resid > 0.075) %>%
  glimpse()
