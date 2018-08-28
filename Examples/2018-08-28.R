library(faraway)
library(tidyverse)
library(visreg)

# Insurance example with offset
motori <- as_tibble(motorins) %>%
  mutate(Kilometres = as.numeric(Kilometres)) %>%
  filter(Zone == 1)

# Log normal model
llm <- lm(log(Payment/Insured) ~ Kilometres + Make + Bonus, motori)
summary(llm)

# Equivalently
llg <- glm(
  log(Payment) ~ offset(log(Insured)) + Kilometres + Make + Bonus,
  family=gaussian ,  motori)
summary(llg)

# Gamma model
gl <- glm(
  Payment ~ offset(log(Insured)) + Kilometres + Make + Bonus,
  family=Gamma(link=log), motori)
summary(gl)

# Tweedie model
help(chredlin)
as_tibble(chredlin)
GGally::ggpairs(chredlin)
chredlin %>% count(involact)

library(mgcv)
twmod <- gam(involact ~ race + fire + theft + age + log(income),
             family=tw(link="log"), data=chredlin)
summary(twmod)

# Plot density with mass at zero:
xgrid <- seq(1e-10, 1.25, len=100)
p <- 1.108
phi <- 0.30847
(mu <- twmod$fit[1])
(poismean <- mu^(2-p)/(phi * (2- p)))
(p0 <- exp(-poismean))
twden <- exp(ldTweedie(xgrid, mu, p=p, phi=phi)[,1])
plot(xgrid, twden*(1-p0), type="l",xlab="x",ylab="Density")
dmax <- max(twden*(1-p0))
segments(0,0,0,dmax,lwd=2)
text(0.08,dmax,paste0("p=",signif(p0,3)))
