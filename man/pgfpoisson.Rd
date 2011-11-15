\name{pgfpoisson}
\alias{pgfpoisson}
\title{
Function pgfpoisson
}
\description{
This function calculates value of the pgf of the Poisson distribution.
}
\usage{
pgfpoisson(s, params)
}
\arguments{
  \item{s}{
Value of the parameter of the pgf. It should be from interval   [-1,1]. In the opposite pgf diverges.
}
  \item{params}{
Positive parameter of the Poisson distribution,  such that  params<-theta.
}
}

\references{
Johnson N, Kotz S, Kemp A (1992) Univariate Discrete Distributions, John Wiley and Sons, New York

http://www.am.qub.ac.uk/users/g.gribakin/sor/Chap3.pdf

}
\author{S. Nadarajah, B. V. Popovic, M. M. Ristic

}
\examples{
params<-5
pgfpoisson(.2,params)

## The function is currently defined as

pgfpoisson <- function(s,params) {
k<-s[abs(s)>1]
if (length(k)>0)
    warning("At least one element of the vector s are out of interval [-1,1]")
if (missing(params)) 
    stop("Distribution parameters are not defined")
    theta<-params[1]
if (theta<=0)
    stop ("Parameter of Poisson distribution must be positive")
    exp(theta*(s-1))
}
}

