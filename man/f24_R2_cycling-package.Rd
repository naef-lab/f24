\name{f24_R2_cycling-package}
\alias{f24_rhythmicity}
\alias{f24_Cycle}
\docType{package}
\title{
What the package does (short line)
To dectect the rhythmicity
}
\description{
More about what it does (maybe more than one line)
blablabla
}
\details{
\tabular{ll}{
Package: \tab f24_R2_cycling\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2013-10-16\cr
License: \tab GPL-2\cr
}
This funciton is to dectect the cycling genes
}
\author{
Jingkui Wang, Felix Naef
<jingkui.wang@epfl.ch>

}
\references{
No references
}
\keyword{ circadian }
\seealso{
None
}
\examples{
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

}
