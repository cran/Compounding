\name{pgfpolyaaeppli}
\alias{pgfpolyaaeppli}
\title{
Function pgfpolyaaeppli
}
\description{
This function calculates value of the pgf of the Polya Aeppli distribution.
}
\usage{
pgfpolyaaeppli(s, params)
}
\arguments{
  \item{s}{
Value of the parameter of the pgf. It should be from interval   [-1,1]. In the opposite pgf diverges.
}
  \item{params}{
List of the parameters of the Polya Aeppli distribution,  such that   params<-c(theta,p), where
theta is positive number, and
p is probability.

}
}

\references{
Johnson N, Kotz S, Kemp A (1992) Univariate Discrete Distributions, John Wiley and Sons, New York

http://www.am.qub.ac.uk/users/g.gribakin/sor/Chap3.pdf

}
\author{S. Nadarajah, B. V. Popovic, M. M. Ristic

}
\examples{
params<-c(5,.6)
pgfpolyaaeppli(.5,params)



## The function is currently defined as

pgfpolyaaeppli <- function(s,params) {
k<-s[abs(s)>1]
if (length(k)>0)
    warning("At least one element of the vector s are out of interval [-1,1]")
if (length(params)<2) 
    stop("At least one value in params is missing")
if (length(params)>2) 
    stop("The length of params is 2")
    theta<-params[1]
    p<-params[2]
if (theta<=0)
    stop ("Parameter theta must be positive")
if ((p>=1)|(p<=0))
    stop ("Parameter p belongs to the interval (0,1)")
exp(theta/p*((1-p)/(1-p*s)-1))
}
}

