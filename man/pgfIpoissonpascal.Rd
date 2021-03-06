\name{pgfIpoissonpascal}
\alias{pgfIpoissonpascal}
\title{
Function pgfIpoissonpascal
}
\description{
This function calculates value of the pgf's inverse of the Poisson-Pascal distribution.
}
\usage{
pgfIpoissonpascal(s, params)
}

\arguments{
  \item{s}{
Value of the parameter of the pgf. It should be from interval   [-1,1]. In the opposite pgf diverges.
}
  \item{params}{
List of the parameters of the Poisson-Pascal distribution,  such that  params<-c(theta,p,k), where all parameters are positive.
}
}

\references{
Johnson N, Kotz S, Kemp A (1992) Univariate Discrete Distributions, John Wiley and Sons, New York

http://www.am.qub.ac.uk/users/g.gribakin/sor/Chap3.pdf

}
\author{S. Nadarajah, B. V. Popovic, M. M. Ristic

}
\examples{
params<-c(5,.4,.3)
pgfIpoissonpascal(.9,params)

## The function is currently defined as

pgfIpoissonpascal <- function(s,params) {
m<-s[abs(s)>1]
if (length(m)>0)
     warning("At least one element of the vector s are out of interval [-1,1]")
if (length(params)<3) 
     stop("At least one value in params is missing")
if (length(params)>3) 
     stop("The length of params is 3")
    theta<-params[1]
    p<-params[2]
    k<-params[3]
if (theta<=0)
     stop ("Parameter theta must be positive")
if (p<=0)
     stop ("Parameter lambda must be positive")
if (k<=0)
     stop ("Parameter k must be positive")
    (1+p-(1+log(s)/theta)^(-1/k))/p
}
}

