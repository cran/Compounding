\name{pgflogarithmicpoisson}
\alias{pgflogarithmicpoisson}
\title{
Function pgflogarithmicpoisson
}
\description{
This function calculates value of the pgf of the logarithmic-Poisson distribution.
}
\usage{
pgflogarithmicpoisson(s, params)
}
\arguments{
  \item{s}{
Value of the parameter of the pgf. It should be from interval   [-1,1]. In the opposite pgf diverges.
}
  \item{params}{
List of the parameters of the logarithmic-Poisson distribution, such that   params<-c(theta,lambda), where 
theta is probability, and
lambda is positive number.
}
}

\references{
Johnson N, Kotz S, Kemp A (1992) Univariate Discrete Distributions, John Wiley and Sons, New York

http://www.am.qub.ac.uk/users/g.gribakin/sor/Chap3.pdf

}
\author{S. Nadarajah, B. V. Popovic, M. M. Ristic

}
\examples{
params<-c(.5,4)
pgflogarithmicpoisson(.5,params)

## The function is currently defined as

pgflogarithmicpoisson <- function(s,params) {
k<-s[abs(s)>1]
if (length(k)>0)
      warning("At least one element of the vector s are out of interval [-1,1]")
if (length(params)<2) 
      stop("At least one value in params is missing")
if (length(params)>2) 
      stop("The length of params is 2")
    theta<-params[1]
    lambda<-params[2]
if ((theta>=1)|(theta<=0))
       stop ("Parameter theta belongs to the interval (0,1)")
if (lambda<=0)
       stop ("Parameter lambda must be positive")
    log(1-(1-theta)*exp(lambda*(s-1)))/log(theta)
}
}

