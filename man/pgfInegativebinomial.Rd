\name{pgfInegativebinomial}
\alias{pgfInegativebinomial}
\title{
Function pgfInegativebinomial
}
\description{
This function calculates value of the pgf's inverse of the negative binomial distribution.
}
\usage{
pgfInegativebinomial(s, params)
}
\arguments{
  \item{s}{
Value of the parameter of the pgf. It should be from interval   [-1,1]. In the opposite pgf diverges.
}
  \item{params}{
List of the parameters of the negative binomial  distribution, such that   params<-c(theta,k), where 
theta is probability, and
k is positive number.
}
}

\references{
Johnson N, Kotz S, Kemp A (1992) Univariate Discrete Distributions, John Wiley and Sons, New York

http://www.am.qub.ac.uk/users/g.gribakin/sor/Chap3.pdf

}
\author{S. Nadarajah, B. V. Popovic, M. M. Ristic

}
\examples{
params<-c(.9,.7)
pgfInegativebinomial(.5,params)


## The function is currently defined as

pgfDnegativebinomial <- function(s,params) {
k<-s[abs(s)>1]
if (length(k)>0)
    warning("Some  elements of the vector s are out of interval [-1,1]")
if (length(params)<2) 
    stop("At least one value in params is missing")
if (length(params)>2) 
    stop("The length of params is 2")
    theta<-params[1]
    k<-params[2]
if ((theta>=1)|(theta<=0))
    stop ("Parameter theta belongs to the interval (0,1)")
if (k<=0)
    stop("Parameter k must be positive")
    k*(1-theta)*theta^k/(1-(1-theta)*s)^(k+1)
}
}

