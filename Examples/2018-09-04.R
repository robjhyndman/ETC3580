library(faraway)
library(tidyverse)
library(broom)
library(visreg)
library(lme4)

# Just use Year 2 students
jspr <- as_tibble(jsp) %>%
  filter(year==2) %>%
  select(-year)

mmod <- lmer(math ~ raven*social + (1|school),  data=jspr)

# Residual diagnostics
augment(mmod) %>%
  ggplot(aes(x=.fitted, y=.resid)) +
  geom_point()
# Clear heteroskedasticity (could transform math scores)

augment(mmod) %>%
  ggplot(aes(sample=.resid)) +
  geom_qq()
# Normality looks ok.

# Check normality of school effects
ranef(mmod) %>% as.tibble %>%
  ggplot(aes(sample=condval)) +
  geom_qq()
# Tails slightly short, but not terrible.

# Plot residuals against predictors
augment(mmod) %>%
  ggplot(aes(x=raven, y=.resid)) +
  geom_point(alpha=0.3) 
augment(mmod) %>%
  ggplot(aes(x=social, y=.resid)) +
  geom_boxplot(alpha=0.3) + theme_bw()
# Both look ok

# Find confidence intervals
confint(mmod)

## Test on interaction. 
drop1(mmod, test="Chisq")
# Not sure we should believe the p value as this is REML
# Try bootstrap instead
# Fit model without interaction
nullmodel <- lmer(math ~ raven + social + (1|school), data=jspr)
# Simulate from null model
nsim <- 1000
lrstat <- numeric(nsim)
for(i in seq(nsim))
{
  y <- simulate(nullmodel)[,1]
  bnull <- lmer(y ~ raven + social + (1|school), data=jspr)
  balt <- lmer(y ~ raven * social + (1|school), data=jspr)
  lrstat[i] <- logLik(balt)-logLik(bnull)
}
# Compute bootstrap p-value
actual <- logLik(mmod)-logLik(nullmodel)
mean(actual < lrstat)

