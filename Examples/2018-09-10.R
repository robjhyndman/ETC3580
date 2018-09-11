library(faraway)
library(tidyverse)
library(broom)
library(visreg)
library(lme4)

# School marks example continued.

# Just use Year 2 students
jspr <- as_tibble(jsp) %>%
  filter(year==2) %>%
  select(-year)

# Model from last week using only nesting within schools
mmod <- lmer(math ~ raven*social + (1|school),  
	data=jspr)

# Fit model for students nested within class and school. 
mmodr <- lmer(
  math ~ raven*social + (1|school) + (1|school:class),  
  data=jspr)
summary(mmodr)

# Compare AIC of various models
# Need to use MLE if using AIC
all3 <- lmer(math ~ raven*social*gender + (1|school) + (1|school:class), 
	data=jspr, REML=FALSE)
all2 <- update(all3, . ~ . - raven:social:gender)
notrs <- update(all2, . ~ . -raven:social)
notrg <- update(all2, . ~ . -raven:gender)
notsg <- update(all2, . ~ . -social:gender)
onlyrs <- update(all2, . ~ . -social:gender - raven:gender)
all1 <-  update(all2, . ~ . -social:gender - raven:gender - social:raven)
nogen <- update(all1, . ~ . -gender)

# Anova shows chisquare tests which are incorrect
# Ignore Chisq tests:
anova(all3, all2, notrs, notrg, notsg, onlyrs, all1, nogen)[,1:4] 

# AIC suggests best model is onlyyrs:
summary(onlyrs)
visreg(onlyrs)

# Model diagnostics
augment(onlyrs) %>%
  ggplot(aes(sample=.resid)) + geom_qq()
augment(onlyrs) %>%
  ggplot(aes(x=.fitted,y=.resid)) + 
    geom_point(alpha=0.3) +
    geom_hline(yintercept=0) +
    xlab("Fitted") + ylab("Residuals")

# Check normality of school:class effects
ranef(onlyrs) %>% as.tibble() %>%
  ggplot(aes(sample=condval)) +
  facet_wrap(~ grpvar) +
  geom_qq() 
# Schools have short tails, class has much smaller variation than schools.

# Extract school effects
# Equal to ranking of schools adjusted for quality of intake
school_rating <- ranef(mmod) %>% 
  as_tibble() %>% 
  filter(grpvar=="school") %>%
  select(condval) %>%
  rename(adjscores = condval)
school_rating

# Compute ranking without adjustment:
school_rating <- school_rating %>%
  bind_cols(rawscores = coefficients(lm(math ~ school-1, data=jspr))) %>%
  mutate(rawscores = scale(rawscores),
         adjscores = scale(adjscores),
         school = seq(length(rawscores)))
school_rating

school_rating  %>%
  ggplot(aes(x=rawscores, y=adjscores, label=school)) +
  geom_point() + geom_abline(intercept=0, slope=1)

library(ggrepel)
school_rating  %>%
  ggplot(aes(x=rawscores, y=adjscores, label=school)) +
  geom_point() + geom_abline(intercept=0, slope=1) +
  geom_text_repel()

# School 14 is best on raw scores, but not after adjustment
# School 37 is middle ranking on raw scores but does well after adjustment
# School 9 is middle ranking on raw scores, but does poorly after adjustment


# Panel data on income dynamics
help(psid)
psid <- as_tibble(faraway::psid)
psid
summary(psid)

# Incomes of first 20 people
psid %>%
  filter(person <= 20) %>%
  ggplot() +
    geom_line(aes(x=year, y=income)) +
    facet_wrap(~ person)

# Take log(x+100) transformation of income
psid %>%
  ggplot() + 
    geom_line(aes(x=year, y=income+100, 
                  group=person)) +
    facet_wrap(~ sex) + 
    scale_y_log10()

# Colour by person
psid %>%
  ggplot() + 
  geom_line(aes(x=year, y=income+100, 
                group=person, col=person)) +
  facet_wrap(~ sex) + 
  scale_y_log10()

# Make color a factor
psid %>%
  ggplot() + 
  geom_line(aes(x=year, y=income+100, 
                group=person, 
                col=as.factor(person))) +
  facet_wrap(~ sex) + 
  scale_y_log10()

# Remove legend
psid %>%
  ggplot() + 
  geom_line(aes(x=year, y=income+100, 
                group=person, 
                col=as.factor(person))) +
  facet_wrap(~ sex) + 
  scale_y_log10() +
  guides(color=FALSE)

# LM on person 1:
lmod <- lm(log(income) ~ I(year-78), 
           subset=(person==1), 
           psid)
coef(lmod)
tidy(lmod)

# Fit LM for all people separately:
psid %>%
  group_by(person) %>%
  do(fit=lm(log(income) ~ I(year-78), data=.)) %>%
  tidy(fit) %>%
  select(person, term, estimate) %>%
  spread(key=term, value=estimate) %>%
  ungroup %>%
  rename(
    intercept = `(Intercept)`,
    slope = `I(year - 78)`
  ) ->
  ml

ml

ggplot(ml) +
  geom_point(aes(x=intercept, y=slope))

demographics <- 
  psid %>% 
  select(age, educ, sex, person) %>%
  distinct

ml <- left_join(ml, demographics)

ml

# Plot coefficients by sex
ggplot(ml) +
  geom_point(aes(x=intercept, y=slope, color=sex))

ggplot(ml) + 
  geom_boxplot(aes(x=sex,y=slope))

# T-tests of sex differences
t.test(slope ~ sex, data=ml)
t.test(intercept ~ sex, data=ml)

# Plot coefficients by education
ggplot(ml) +
  geom_point(aes(x=intercept, y=slope, 
                 color=as.factor(educ)))

# Plot coefficients by age
ggplot(ml) +
  geom_point(aes(x=intercept, y=slope, color=as.factor(age)))

# Now do random effects model with other predictors
mmod <- lmer(
  log(income) ~ year*sex + age + educ + (year|person),
  data=psid)
summary(mmod)
# Numerical issues. Centre year.

psid <- mutate(psid, cyear = year-78)
mmod <- lmer(
  log(income) ~ cyear*sex + age + educ + (cyear|person),
  data=psid)
summary(mmod)
visreg(mmod)

