library(faraway)
library(tidyverse)
library(KernSmooth)

# Fix exb data
exb <- as.tibble(exb)

# Local constant (kernel smoothing)

fit <- locpoly(faithful$eruptions, faithful$waiting,
               degree=0, bandwidth=0.3) %>% as.tibble
ggplot(faithful) +
  geom_point(aes(x=eruptions,y=waiting)) +
  geom_line(data=fit, aes(x=x,y=y), col='blue')

fit <- locpoly(exa$x, exa$y,
               degree=0, bandwidth=0.03) %>% as.tibble
ggplot(exa) +
  geom_point(aes(x=x,y=y)) +
  geom_line(data=fit, aes(x=x,y=y), col='blue') +
  geom_line(aes(x=x,y=m), col='red')

fit <- locpoly(exb$x, exb$y,
               degree=0, bandwidth=0.05) %>% as.tibble
ggplot(exb) +
  geom_point(aes(x=x,y=y)) +
  geom_line(data=fit, aes(x=x,y=y), col='blue') +
  geom_line(aes(x=x,y=m), col='red')

# Local linear

fit <- locpoly(faithful$eruptions, faithful$waiting,
               degree=1, bandwidth=0.3) %>% as.tibble
ggplot(faithful) +
  geom_point(aes(x=eruptions,y=waiting)) +
  geom_line(data=fit, aes(x=x,y=y), col='blue')

h <- dpill(faithful$eruptions,faithful$waiting)
fit <- locpoly(faithful$eruptions, faithful$waiting,
               degree=1, bandwidth=h) %>% as.tibble
ggplot(faithful) +
  geom_point(aes(x=eruptions,y=waiting)) +
  geom_line(data=fit, aes(x=x,y=y), col='blue')

h <- dpill(exa$x,exa$y)
fit <- locpoly(exa$x, exa$y,
               degree=1, bandwidth=h) %>% as.tibble
ggplot(exa) +
  geom_point(aes(x=x,y=y)) +
  geom_line(data=fit, aes(x=x,y=y), col='blue') +
  geom_line(aes(x=x,y=m), col='red')

h <- dpill(exb$x, exb$y)
fit <- locpoly(exb$x, exb$y,
               degree=1, bandwidth=h) %>% as.tibble
ggplot(exb) +
  geom_point(aes(x=x,y=y)) +
  geom_line(data=fit, aes(x=x,y=y), col='blue') +
  geom_line(aes(x=x,y=m), col='red')

# Local quadratic

fit <- locpoly(faithful$eruptions, faithful$waiting,
               degree=2, bandwidth=0.6) %>% as.tibble
ggplot(faithful) +
  geom_point(aes(x=eruptions,y=waiting)) +
  geom_line(data=fit, aes(x=x,y=y), col='blue')

fit <- locpoly(exa$x, exa$y,
               degree=2, bandwidth=0.05) %>% as.tibble
ggplot(exa) +
  geom_point(aes(x=x,y=y)) +
  geom_line(data=fit, aes(x=x,y=y), col='blue') +
  geom_line(aes(x=x,y=m), col='red')

fit <- locpoly(exb$x, exb$y,
               degree=2, bandwidth=0.06) %>% as.tibble
ggplot(exb) +
  geom_point(aes(x=x,y=y)) +
  geom_line(data=fit, aes(x=x,y=y), col='blue') +
  geom_line(aes(x=x,y=m), col='red')

## Loess

smr <- loess(waiting ~ eruptions, data=faithful)
ggplot(faithful) +
  geom_point(aes(x=eruptions,y=waiting)) +
  ggtitle("Old Faithful (Loess, span=0.75)") +
  geom_line(aes(x=eruptions, y=fitted(smr)), col='blue')

smr <- loess(y ~ x, data=exa, span=0.22)
ggplot(exa) +
  geom_point(aes(x=x,y=y)) +
  ggtitle("Example A (Loess, span=0.22)") +
  geom_line(aes(x=x, y=fitted(smr)), col='blue') +
  geom_line(aes(x=x,y=m), col='red')

smr <- loess(y ~ x, data=exb, family='symmetric')
ggplot(as.data.frame(exb)) +
  geom_point(aes(x=x,y=y)) +
  ggtitle("Example B (Robust Loess, span=0.75)") +
  geom_line(aes(x=x, y=fitted(smr)), col='blue') +
  geom_line(aes(x=x,y=m), col='red')

