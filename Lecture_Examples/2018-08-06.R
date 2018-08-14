library(faraway)
library(tidyverse)
library(visreg)

#### BINARY REGRESSION

# Coronary Heart Disease
help(wcgs)
wcgs <- as_tibble(wcgs)

wcgs

wcgs %>%
  select(chd, height, cigs) %>%
  summary()

ggplot(wcgs, aes(x=chd, y=height)) +
  geom_boxplot()

ggplot(wcgs, aes(x=chd, y=height)) + 
  geom_point() +
  ylab("Heart disease") + xlab("Height")

ggplot(wcgs, aes(x=chd, y=height)) + 
  geom_jitter(width=0.05, height=0) +
  ylab("Heart disease") + xlab("Height")

ggplot(wcgs, aes(x=height, fill=chd)) +
  geom_histogram(binwidth=1)

ggplot(wcgs, aes(x=cigs, fill=chd)) +
  geom_histogram(binwidth=5,boundary=0)

ggplot(wcgs, aes(x=height,y=cigs)) +
  geom_point(alpha=0.2, position=position_jitter()) +
  facet_grid(~ chd)

lmod <- glm(chd ~ height + cigs, family = binomial, wcgs)
summary(lmod)

## Visualize
visreg(lmod, "height", gg=TRUE) + theme_bw()
visreg(lmod, "cigs", gg=TRUE) + theme_bw()

## Predictions
library(broom)
wcgs_aug <- augment(lmod, type.predict='response')

ggplot(wcgs_aug, aes(x=height, y=.fitted, col=chd)) +
  geom_point(alpha=0.3)
ggplot(wcgs_aug, aes(x=cigs, y=.fitted, col=chd)) +
  geom_point(alpha=0.3)

## Predictions on logit scale
wcgs_aug <- augment(lmod, type.predict='link')
ggplot(wcgs_aug, aes(x=height, y=.fitted, col=chd)) +
  geom_point(alpha=0.3)
ggplot(wcgs_aug, aes(x=cigs, y=.fitted, col=chd)) +
  geom_point(alpha=0.3)

