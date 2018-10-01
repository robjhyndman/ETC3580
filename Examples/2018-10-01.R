library(faraway)
library(tidyverse)
library(KernSmooth)
library(MASS)
library(mgcv)
library(visreg)
library(splines)

## Inference

ggplot(mcycle, aes(x=times, y=accel)) +
  geom_point() +
  geom_smooth(span=0.2)

ggplot(mcycle, aes(x=times, y=accel)) +
  geom_point() +
  geom_smooth(method='gam',
              formula = y ~ s(x))

fit1 <- gam(accel ~ s(times), data=mcycle)
fit2 <- gam(accel ~ times, data=mcycle)
anova(fit1,fit2, test="F")


