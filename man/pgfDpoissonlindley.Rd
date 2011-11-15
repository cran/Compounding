\name{pgfDpoissonlindley}
\alias{pgfDpoissonlindley}
%\alias{pgfpoissonlindley}
\title{
Function pgfDpoissonlindley.
}
\description{
This function calculates value of the pgf's first derivative of the Poisson-Lindley distribution.
}
\usage{
pgfDpoissonlindley(s, params)
}
\arguments{
  \item{s}{
Value of the parameter of the pgf. It should be from interval  [-1,1]. In the opposite pgf divegrates.
}
  \item{params}{
Positive parameter of the  Poisson-Lindley distribution,  such that  params<-theta.
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
pgfDpoissonlindley(.5,params)

## The function is currently defined as

pgfDpoissonlindley <- function(s,params) {
k<-s[abs(s)>1]
if (length(k)>0)
    warning("At least one element of the vector s are out of interval [-1,1]")
if (length(params)>1) 
    stop("The length of params is 1")
    theta<-params[1]
if (theta<=0)
    stop ("Parameter lambda must be positive")
    (2/(theta+1-s)-1/(theta+2-s))*pgfpoissonlindley(s,params)
}
}

