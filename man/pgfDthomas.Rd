\name{pgfDthomas}
\alias{pgfDthomas}
\title{
Function pgfDthomas
}
\description{
This function calculates value of the pgf of the Thomas distribution.
}
\usage{
pgfDthomas(s, params)
}
\arguments{
  \item{s}{
Value of the parameter of the pgf. It should be from interval   [-1,1]. In the opposite pgf divegrates.
}
  \item{params}{
List of the parameters of the Thomas distribution,  such that   params<-c(lambda,theta), where
both parameters are  positive.

}
}

\references{
Johnson N, Kotz S, Kemp A (1992) Univariate Discrete Distributions, John Wiley and Sons, New York

http://www.am.qub.ac.uk/users/g.gribakin/sor/Chap3.pdf

}
\author{S. Nadarajah, B. V. Popovic, M. M. Ristic

}
\examples{
params<-c(5,.4)
pgfDthomas(.5,params)

## The function is currently defined as

pgfDthomas <- function(s,params) {
k<-s[abs(s)>1]
if (length(k)>0)
    warning("At least one element of the vector s are out of interval [-1,1]")
if (length(params)<2) 
    stop("At least one value in params is missing")
if (length(params)>2) 
    stop("The length of params is 2")
    lambda<-params[1]
    theta<-params[2]
if (lambda<=0)
    stop ("Parameter lambda must be positive")
if (theta<=0)
    stop ("Parameter theta must be positive")
    lambda*(1+theta*s)*exp(theta*(s-1))*exp(lambda*(s*exp(theta*(s-1))-1))
}
}

