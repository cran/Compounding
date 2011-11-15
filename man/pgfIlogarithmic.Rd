\name{pgfIlogarithmic}
\alias{pgfIlogarithmic}
\title{
Function pgfIlogarithmic
}
\description{
This function calculates value of the pgf's inverse of the logarithmic distribution.
}
\usage{
pgfIlogarithmic(s, params)
}
\arguments{
  \item{s}{
Value of the parameter of the pgf. It should be from interval   [-1,1]. In the opposite pgf diverges.
}
  \item{params}{
Paramemeter of logarithmic distribution, such that           params<-theta, where theta is probability.
}
}

\references{
Johnson N, Kotz S, Kemp A (1992) Univariate Discrete Distributions, John Wiley and Sons, New York

http://www.am.qub.ac.uk/users/g.gribakin/sor/Chap3.pdf

}
\author{S. Nadarajah, B. V. Popovic, M. M. Ristic

}

\examples{
params<-.7
pgfIlogarithmic(.5,params)

 ## The function is currently defined as

pgfIlogarithmic <- function(s,params) {
k<-s[abs(s)>1]
if (length(k)>0)
     warning("At least one element of the vector s are out of interval [-1,1]")
if (length(params)>1) 
    stop("The length of params is 1")
    theta<-params[1]
if ((theta>=1)|(theta<=0))
    stop ("Parameter theta belongs to the interval (0,1)")
    (1-theta^s)/(1-theta)
}
}

