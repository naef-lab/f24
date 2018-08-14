# f24: test time series data for rhythmicity using harmonic regression

Example

```
library(f24.R2.cycling)
tvec <- seq(0, 48, 2)
jphase <- 6
jmean <- 10
jperiod <- 24
jnoise <- 0.2
jomega <- 2 * pi / jperiod
x <- jmean + cos(jomega * (tvec - jphase)) + rnorm(n = length(tvec), sd = jnoise)
plot(tvec, x)
fit.x <- f24_R2_cycling(x, t=tvec, period = jperiod)
tvec.pred <- seq(0, 48, length.out = 50)
x.pred <- get_curve_from_fit(fit.x, tvec.pred)
lines(tvec.pred, x.pred)
```
