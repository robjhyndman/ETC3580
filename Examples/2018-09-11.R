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


## Binary response
## Effect of surface and vision on balance

help(ctsib)
summary(ctsib)

# Add stable variable (CTSIB==1)
ctsib <- faraway::ctsib %>%
  as_tibble %>%
  mutate(stable = (CTSIB==1),
         Subject= as.factor(Subject))

summary(ctsib)

# Look at mean responses:
ctsib %>%
  group_by(Vision, Surface) %>%
  summarize(mean.stable = mean(stable))

# Group by subject and average over the 12 observations
ctsib %>%
  group_by(Subject) %>%
  summarise(Height=Height[1],  Weight=Weight[1],
            mean.stable=mean(stable), Age=Age[1], Sex=Sex[1]) ->
  subsum

ggplot(subsum) +
  geom_point(aes(x=Height,y=mean.stable))

ggplot(subsum) +
  geom_point(aes(x=Weight,y=mean.stable))

ggplot(subsum) +
  geom_point(aes(x=Age,y=mean.stable))

ggplot(subsum) +
  geom_jitter(aes(x=Sex,y=mean.stable), height=0, width=0.1, alpha=0.3)

ggplot(subsum) +
  geom_boxplot(aes(x=Sex,y=mean.stable))

# Suppose we ignore subject effect and just fit GLM:
gf <- glm(stable ~ Sex + Age + Height + Weight +
            Surface + Vision,
          family=binomial, data=ctsib)
summary(gf)
# This will underestimate standard errors --> spurious significance

# Or we could treat subject as fixed effect
gfs <- glm(stable ~ Sex + Age + Height + Weight +
            Surface + Vision + Subject,
          family=binomial, data=ctsib)
# Convergence issues.
summary(gfs)
# Not all effects estimate. Other estimates effectively infinite!

## Do it pseudo-properly with random subject effects
modpql <- MASS::glmmPQL(stable ~ Sex + Age + Height + Weight + Surface + Vision,
                  random=~1|Subject,  family=binomial, data=ctsib)
summary(modpql)
## Interpretation of subject effect:
# exp(3.06) = 21.3. So a 1SD change in subject effect increases
# odds of stability by 21.3x.
# Residual SD is meaningless.
# Surface=norm and Vision=open have strongest positive effects on stability

## Re-fit with numerical integration of likelihood
## using lme4::glmer
modlap <- glmer(stable ~ Sex + Age + Height + Weight + Surface +  Vision +
                  (1|Subject), family=binomial, data=ctsib)
##Hmm. Nasty warnings!
## increase accuracy of approximation by increasing nAGQ value
modgh <- glmer(stable ~ Sex + Age + Height + Weight + Surface +  Vision +
                 (1|Subject), nAGQ=25, family=binomial, data=ctsib)
## Slow and still got warnings
summary(modgh)
## Model nearly unidentifiable. Yuk.
### However, parameters nearly the same as for PQL. So maybe ok?
## Let's drop least significant variables
modgh2 <- glmer(stable ~ Surface +  Vision + (1|Subject),
                nAGQ=25, family=binomial, data=ctsib)
summary(modgh2)
visreg(modgh2)

anova(modgh, modgh2)

# Chisq test suggests subject-specific variables not required.
# But be skeptical as the asymptotics could be dodgy here.

augment(modgh2) %>%
  ggplot(aes(sample=.resid)) +
  geom_qq() +
  facet_grid(Surface~Vision)

## Fitting problems showing up as residuals for two combinations
## being essentially 0. i.e., these combinations almost always
## predicted unstable, so no error.
## Not the same as heteroskedasticity



# Count response
## Trial of 59 epileptics.
## Patients observed for 8 weeks, then randomized to treatment group.
## Observed for four additional 2-week periods
## Response is number of seizures in each period.

help(epilepsy)

# Add some structural variables:
faraway::epilepsy %>%
  as_tibble %>%
  mutate(
    period = rep(0:4, 59),
    drug = ifelse(treat==1, "treatment", "placebo"),
    phase = ifelse(expind==0, "baseline", "experiment"),
    id = as.factor(id)
  ) %>%
  select(id,period,seizures,timeadj,drug,phase) ->
  epilepsy

# Check first two patients
epilepsy

# Mean seizures per week for each combination of drug and phase
epilepsy %>%
  group_by(drug, phase) %>%
  summarise(rate=mean(seizures/timeadj))
# Rate of seizures goes up in both groups
# But more in placebo group

# Plot number of seizures over time for each person
ggplot(epilepsy) +
  geom_line(aes(x=period, y=seizures/timeadj, 
                col=id, group=id)) +
  facet_grid( ~ drug) +
  scale_y_sqrt(breaks=(0:10)^2) +
  guides(col=FALSE)

# Would be easier if we permuted the colours
library(scales)
nid <- length(levels(epilepsy$id))
cols <- sample(hue_pal()(nid))
ggplot(epilepsy) +
  geom_line(aes(x=period, y=seizures/timeadj, col=id, group=id)) +
  facet_grid( ~ drug) +
  scale_y_sqrt(breaks=(0:10)^2) +
  scale_color_manual(values=cols) +
  guides(col=FALSE)

# Averages for each id in baseline and experiment
epilepsy %>%
  group_by(id, phase, drug) %>%
  summarise(rate=mean(seizures/timeadj)) %>%
  spread(phase, rate) ->
  comsum

ggplot(comsum) +
  geom_point(aes(x=baseline, y=experiment, col=drug)) +
  scale_x_sqrt() + scale_y_sqrt() +
  geom_abline(intercept=0, slope=1)
# No obvious treatment effect

# One large outlier which will be high leverage
filter(comsum, experiment > 30)
# Possibly best to omit and study separately
epilo <- filter(epilepsy, id != 49)

# First try a GLM ignoring subject effects
# Need an offset to adjust for length of periods
modglm <- glm(seizures ~ offset(log(timeadj)) + 
                drug * phase,
                family=poisson, data=epilo)
summary(modglm)
# Positive phase effect -- everyone got more seizures in treatment phase
# Main effect for treatment includes baseline period
# So negative treatment effect is not interesting
# Interaction the main thing of interest here.
# Interaction shows treatment x experiment effect is negative (good!)
# But p-values too small due to ignoring subject effects

# Now try a GLMM
modpql <- MASS::glmmPQL(seizures ~ offset(log(timeadj)) + 
                    drug * phase,
                    random = ~1|id, 
                  family=poisson, data=epilo)
summary(modpql)
# Once we take the grouping into account it looks like the
# interaction is still significant. Other effects not significant
# Looks like the drug is doing something useful

modgh <- glmer(seizures ~ offset(log(timeadj)) + 
                 drug * phase + (1|id),
               nAGQ=25, family=poisson, data=epilo)
summary(modgh)
# Much the same coefficients, although p-values smaller
# Same conclusions hold
# Size of effect of drug:
exp(-0.302) - 1

# So seizures reduced by 26%
# Much smaller than variation between individuals

