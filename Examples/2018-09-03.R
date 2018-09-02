library(faraway)
library(tidyverse)
library(broom)
library(visreg)
library(lme4)

help(jsp)
head(jsp)
# Just use Year 2 students
jspr <- as_tibble(jsp) %>%
  filter(year==2) %>%
  select(-year)

summary(jspr)

jspr %>% 
  select(-id, -school) %>%
  GGally::ggpairs()

ggplot(jspr) + 
  geom_jitter(aes(x=raven, y=math), alpha=0.3) +
  xlab("Raven Score") + 
  ylab("Math Score")
  
ggplot(jspr) + 
  geom_boxplot(aes(x=social, y=math)) +
  xlab("Social Class") + 
  ylab("Math Score")

# Regular linear model with fixed effects
# and up to two way interactions
lin <- lm(math ~ raven * social, jspr)
summary(lin)
anova(lin)

visreg(lin, "raven", by="social", gg=TRUE) + theme_bw()

# But this ignores grouping within classes and schools
# Let's add a school random effect (but ignore classes for now)
mmod <- lmer(math ~ raven*social + (1|school),  data=jspr)
summary(mmod)
visreg(mmod, "raven", by="social", gg=TRUE) + theme_bw()

# Same model with MLE rather than REML
mmod_mle <- lmer(math ~ raven*social + (1|school),  REML=FALSE, data=jspr)
summary(mmod_mle)
# Almost no difference in this case

# Random effects
ranef(mmod)

# Find confidence intervals
confint(mmod)

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

## Test on interaction. This will refit using MLE.
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

