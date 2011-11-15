\name{pgfIgeometric}
\alias{pgfIgeometric}
\title{
Function pgfIgeometric
}
\description{
This function calculates value of the pgf's inverse of the geometric distribution.
}
\usage{
pgfIgeometric(s, params)
}
\arguments{
  \item{s}{
Value of the parameter of the pgf. It should be from interval   [-1,1]. In the opposite pgf diverges.
}
  \item{params}{
Parameter of the geometric distribution, such that   params<-theta, where
theta is probability.
}
}

\references{
Johnson N, Kotz S, Kemp A (1992) Univariate Discrete Distributions, John Wiley and Sons, New York

http://www.am.qub.ac.uk/users/g.gribakin/sor/Chap3.pdf

}
\author{S. Nadarajah, B. V. Popovic, M. M. Ristic

}
\examples{
params<-.1
pgfIgeometric(.5,params)

## The function is currently defined as

pgfIgeometric <- function(s,params) {
k<-s[abs(s)>1]
if (length(k)>0)
     warning("At least one element of the vector s are out of interval [-1,1]")
if (length(params)>1) 
     stop("The length of params is 1")
   theta<-params[1]
if ((theta>=1)|(theta<=0))
     stop ("Parameter of geometric distribution must belong  to the interval (0,1)")
    (s-theta)/((1-theta)*s)
}
}

