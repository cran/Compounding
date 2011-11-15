\name{pgfIbinomialpoisson}
\alias{pgfIbinomialpoisson}
\title{
Function pgfIbinomialpoisson
}

\description{
This function calculates value the pgf's inverse of the binomial-Poisson distribution.
}
\usage{
pgfIbinomialpoisson(s, params)
}
\arguments{
  \item{s}{
Value of the parameter of the pgf. It should be from interval   [-1,1]. In the opposite pgf diverges.
}
  \item{params}{
List of the parameters of the binomial-Poisson distribution,  such that   params<-c(theta,p,n), where
theta is positive number,
p is probability, and
n is positive integer.
}
}

\references{
Johnson N, Kotz S, Kemp A (1992) Univariate Discrete Distributions, John Wiley and Sons, New York

http://www.am.qub.ac.uk/users/g.gribakin/sor/Chap3.pdf

}
\author{S. Nadarajah, B. V. Popovic, M. M. Ristic

}
\examples{
params<-c(.4,.9,5)
pgfIbinomialpoisson(.5,params)

## The function is currently defined as

pgfIbinomialpoisson <- function(s,params) {
k<-s[abs(s)>1]
if (length(k)>0)
   warning("At least one element of the vector s are out of interval [-1,1]")
if (length(params)<3) 
   stop("At least one value in params is missing")
if (length(params)>3) 
   stop("The length of params is 3")
    theta<-params[1]
    p<-params[2]
    n<-params[3]
if (theta<=0)
   stop ("Parameter theta must be positive")
if ((p>=1)|(p<=0))
   stop ("Parameter p belongs to the interval (0,1)")
if (n<0)
     stop("Parameter n must be positive")
 if(!(abs(n-round(n))<.Machine$double.eps^0.5))
stop("Parameter n must be positive integer")
    zval<-(s^(1/n)-1+p)/p
    1+log(zval)/theta
}
}

