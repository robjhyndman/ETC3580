library(faraway)
library(tidyverse)
library(visreg)

# Insurance example with offset
help(motorins)
motori <- as_tibble(motorins)
motori
summary(motori)
motori <- motori %>%
  mutate(Kilometres = as.numeric(Kilometres)) %>%
  filter(Zone == 1)
summary(motori)

GGally::ggpairs(motori)

motori %>%
  mutate(Payment = log(Payment),
         Insured= log(Insured)) %>%
  GGally::ggpairs()

# Log normal model
llm <- lm(log(Payment/Insured) ~ Kilometres + Make + Bonus, motori)
summary(llm)

# Equivalently
llg <- glm(
  log(Payment) ~ offset(log(Insured)) + Kilometres + Make + Bonus,
  family=gaussian ,  motori)
summary(llg)

