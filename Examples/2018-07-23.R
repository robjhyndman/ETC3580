library(faraway)
library(tidyverse)

#############################################
# gavote example
#############################################

# Use tibbles rather than data frames
gavote <- as.tibble(gavote)

# To find out what things are
help(gavote)

# Type name of object to see it
gavote

# Summarize an object
summary(gavote)

# Quick look at data
GGally::ggpairs(gavote)

# Rename variable
gavote %>%
  rename(usage = rural) ->
  gavote

# Create new variables
gavote %>%
  mutate(
    undercount = (ballots - votes) / ballots,
    pergore = gore / votes,
    county = rownames(faraway::gavote)
  ) ->
  gavote

# A simpler pairs plot
gavote %>%
  select(equip, econ, usage, atlanta, perAA, pergore, undercount) %>%
  GGally::ggpairs()

# Summarize individual variable
gavote %>%
  pull(undercount) %>%
  summary()

# Calculation using variables within tibble
gavote %>%
  summarise(sum(ballots - votes) / sum(ballots))

# Histogram
ggplot(gavote, aes(undercount)) +
  geom_histogram()

# Histogram with tweaks
ggplot(gavote, aes(undercount)) +
  geom_histogram(
    bins = nclass.FD(gavote$undercount),
    boundary = 0
  ) +
  geom_rug() +
  ggtitle("Undercount") +
  xlab("Percent Undercount")

# Bar plot
ggplot(gavote, aes(equip)) +
  geom_bar()

# Scatterplot
ggplot(gavote, aes(x = perAA, y = pergore)) +
  geom_point() +
  xlab("Proportion African American") +
  ylab("Proportion for Gore")

# Boxplot
ggplot(gavote, aes(x = equip, y = undercount)) +
  geom_boxplot()

# Cross-tabulation
gavote %>%
  count(atlanta, usage) %>%
  spread(usage, n)

# Correlation matrix for some variables
gavote %>%
  select(perAA, ballots, undercount, pergore) %>%
  cor()

# Pairwise scatterplots
gavote %>%
  select(perAA, ballots, undercount, pergore) %>%
  GGally::ggpairs()

# Linear model
lmod <- lm(undercount ~ pergore + perAA, 
           data=gavote)

# Bigger Linear model
lmod <- lm(undercount ~ pergore + perAA + equip + econ + usage + atlanta, 
           data=gavote)

# Base helper functions with linear model
summary(lmod)
coef(lmod)
predict(lmod)
residuals(lmod)

