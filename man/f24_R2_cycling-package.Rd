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
t = c(0:48) 
x = 10 + cos(2*pi/24*(t-12))
f24_R2_cycling(x,t)

}
