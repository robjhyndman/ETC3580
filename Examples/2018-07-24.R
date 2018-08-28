library(faraway)
library(tidyverse)

#############################################
# gavote example
#############################################

gavote %>%
  as.tibble()
  rename(usage = rural) %>%
  mutate(
    undercount = (ballots - votes) / ballots,
    pergore = gore / votes,
    county = rownames(faraway::gavote)
  ) ->
  gavote

# Bigger Linear model
lmod <- lm(undercount ~ pergore + perAA + equip + econ + usage + atlanta,
           data=gavote)
summary(lmod)

# Broom helper functions
library(broom)
tidy(lmod)
augment(lmod)
glance(lmod)

# Visualization
library(visreg)
visreg(lmod)
visreg(lmod, "pergore", gg=TRUE) + theme_bw()
visreg(lmod, "perAA", gg=TRUE) + theme_bw()

# Interactions
# Can we spot interaction of perAA and equip in data?
gavote %>%
  ggplot(aes(x=perAA, y=undercount, group=equip)) +
  geom_point() + facet_grid(~equip)
lmod2 <- lm(undercount ~ pergore + perAA*equip + econ + usage + atlanta,
            data=gavote)
summary(lmod2)
visreg(lmod2, 'perAA', by='equip', gg=TRUE) + theme_bw()
# Much easier to see after conditioning on other variables

# Hypothesis testing
anova(lmod2)
drop1(lmod2, test="F")
# Last row is test of interaction only, not main effects.

# Comparing two models
anova(lmod,lmod2)

# Confidence interval
confint(lmod)

confint(lmod2)

###############################################
## airquality example with polynomials
###############################################

help(airquality)
GGally::ggpairs(airquality)
airquality %>%
  select(Solar.R, Wind, Temp, Ozone) %>%
  GGally::ggpairs()

# Looks like some nonlinear relationships, especially with wind and temp
# Try to spot interaction between Temp and Wind on Ozone
airquality %>%
  mutate(
    Heat = cut(Temp, 3, labels = c("Cool", "Mild", "Hot"))
  ) %>%
  ggplot(aes(x=Wind, y=Ozone, group=Heat)) +
    geom_point() +
    facet_wrap(~Heat)
# Maybe?

# Start with quadratic main effects model
fit <- lm(Ozone ~ Solar.R + Wind + Temp + I(Wind^2) + I(Temp^2),
          data = airquality)
summary(fit)
visreg(fit, "Solar.R", gg=TRUE) + theme_bw()
visreg(fit, "Wind", gg=TRUE) + theme_bw()
visreg(fit, "Temp", gg=TRUE) + theme_bw()

# Compare orthogonal polynomial version of the same model
fit2 <- lm(Ozone ~ Solar.R + poly(Wind,2) + poly(Temp,2),
          data = airquality)
summary(fit2)

