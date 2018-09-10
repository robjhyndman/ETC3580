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
