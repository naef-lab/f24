\name{f24_R2_cycling}
\alias{f24_R2_cycling}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{f24.R2.cycling
%%  ~~function to do ... ~~
}
\description{This function provides a statistical test of rhythmicity for temporal accumulations of mRNA, proteins and other biological signals.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
f24_R2_cycling(x, t = 2 * (0:(length(x) - 1)), period = 24, offset = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{numerical data of temporal accumulation
%%     ~~Describe \code{x} here~~
}
  \item{t}{time points
%%     ~~Describe \code{t} here~~
}
  \item{period}{period of rhythmicity
%%     ~~Describe \code{period} here~~
}
  \item{offset}{offset ususally is 0
%%     ~~Describe \code{offset} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{nb.timepoints }{number of time points}
 \item{mean}{average}
 \item{amp}{amplitude}
 \item{relamp}{relative amplitude}
 \item{phase}{phase}
 \item{pval}{P values}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{Jingkui Wang and Felix Naef
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## t = c(0:48)
## x = 10 + cos(2*pi/24*(t-12))
## f24_R2_cycling(x,t)
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, t = 2 * (0:(length(x) - 1)), period = 24, offset = 0) 
{
    kk = which(!is.na(x) == TRUE)
    x = x[kk]
    t = t[kk]
    n = length(x)
    nb.timepoints = length(x)
    if (n < 4) {
        if (n == 0) 
            c(nb.timepoints = nb.timepoints, mean = NA, amp = NA, 
                relamp = NA, phase = NA, pval = NA)
        else {
            c(nb.timepoints = nb.timepoints, mean = mean(x), 
                amp = NA, relamp = NA, phase = NA, pval = NA)
        }
    }
    else {
        sig2 = var(x)
        c = cos(2 * pi * t/period)
        s = sin(2 * pi * t/period)
        A = mean(x * c) - mean(x) * mean(c)
        B = mean(x * s) - mean(x) * mean(s)
        c1 = mean(c^2) - mean(c)^2
        c2 = mean(c * s) - mean(c) * mean(s)
        c3 = mean(s^2) - mean(s)^2
        b = (A * c2 - B * c1)/(c2^2 - c1 * c3)
        a = (A - b * c2)/c1
        mu = mean(x) - a * mean(c) - b * mean(s)
        x.hat = mu + a * c + b * s
        sig2.1 = var(x - x.hat)
        if (is.na(a) || is.na(b)) {
            c(lu = lu, mean = mean(x), amp = NA, relamp = NA, 
                phase = NA, pval = NA)
        }
        else {
            p = 3
            R2 = 0
            if (sig2 > 0) 
                R2 = 1 - sig2.1/sig2
            amp = max(x) - min(x)
            phase = period/(2 * pi) * atan2(b, a)
            if (phase < 0) 
                phase = phase + period
            if (phase > period) 
                phase = phase - period
            phase = (phase + offset)\%\%period
            pval = pbeta(R2, (p - 1)/2, (n - p)/2, lower.tail = FALSE, 
                log.p = FALSE)
            c(nb.timepoints = nb.timepoints, mean = mean(x), 
                amp = 2 * sqrt(a^2 + b^2), relamp = sqrt(a^2 + 
                  b^2)/(mu), phase = phase, pval = pval)
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
