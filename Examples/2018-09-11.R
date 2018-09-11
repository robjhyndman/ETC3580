library(faraway)
library(tidyverse)
library(broom)
library(visreg)
library(lme4)


# Panel data on income dynamics
psid <- as_tibble(faraway::psid) %>%
  mutate(cyear = year-78)
psid

mmod <- lmer(
  log(income) ~ cyear*sex + age + educ + (cyear|person),
  data=psid)
visreg(mmod, "cyear", by='sex', gg=TRUE) + theme_bw()
summary(mmod)

# Write out model

# Incomes of men are about exp(1.15) = 3.16 times higher
# Female income increases about 8.5% per year
# Male income increases about 8.5-2.6=5.9% per year

confint(mmod, method="boot")
# Age not significant

# Diagnostic plots
augment(mmod) %>%
  ggplot(aes(x=.fitted,y=.resid)) +
  geom_point(, alpha=0.3) + 
  geom_hline(yintercept=0) + 
  xlab("Fitted") + ylab("Residuals")

augment(mmod) %>%
  ggplot(aes(sample=.resid)) +
  geom_qq() + facet_grid(~sex)
# Very non-normal!
# Long tail for lower incomes and greater variance for females
# Probably don't want a log then -- maybe try other transformations?
# Better still, we will use a nonparametric function of income (later)

