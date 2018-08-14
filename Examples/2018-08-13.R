library(faraway)
library(tidyverse)
library(visreg)
library(broom)

## Binomial regression

orings
?orings

as_tibble(orings) %>%
  mutate(
    undamaged=6-damage, 
    pdamage = damage/6) ->
  orings

orings

summary(orings)

ggplot(orings, aes(x=temp, y=pdamage)) +
  geom_jitter(height=0.02) +
  xlab("Temperature") + ylab("Prob of damage")

lmod <- glm(cbind(damage,undamaged) ~ temp,
            family=binomial, orings)
summary(lmod)
drop1(lmod, test="Chisq")
confint(lmod)

newdf <- tibble(
  temp = 30:85,
  pred = predict(lmod, 
           newdata=data.frame(temp=temp),
           type='response')
)

ggplot(orings, aes(x=temp, y=pdamage)) +
  geom_jitter(height=0.02) +
  xlab("Temperature") + ylab("Prob of damage") +
  geom_line(aes(x=temp, y=pred), data=newdf)

# Try quadratic

lmod2 <- glm(cbind(damage,undamaged) ~ temp + I(temp^2),
             family=binomial, orings)
summary(lmod2)
drop1(lmod2, test="Chisq")

newdf2 <- tibble(
  temp = 30:85,
  pred = predict(lmod2, 
        newdata=data.frame(temp=temp),
        type='response')
)

ggplot(orings, aes(x=temp, y=pdamage)) +
  geom_jitter(height=0.02) +
  xlab("Temperature") + ylab("Prob of damage") +
  geom_line(aes(x=temp, y=pred), col='red', data=newdf) +
  geom_line(aes(x=temp, y=pred), col='blue', data=newdf2)

# Look for overdispersion

summary(lmod)
# Dev (16.9) < m-p (21). So no problem.
# But here is how to deal with it if needed:

lmod3 <- glm(cbind(damage,undamaged) ~ temp, 
             family=quasibinomial, orings)
summary(lmod3)

