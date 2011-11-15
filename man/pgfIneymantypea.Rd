\name{pgfIneymantypea}
\alias{pgfIneymantypea}
\title{
Function pgfIneymantypea
}
\description{
This function calculates value of the pgf's inverse of the Neyman type A distribution.
}
\usage{
pgfIneymantypea(s, params)
}
\arguments{
  \item{s}{
Value of the parameter of the pgf. It should be from interval    [-1,1]. In the opposite pgf diverges.
}
  \item{params}{
List of the parameters of the Neyman type A distribution, such that   params<-c(theta,lambda), where 
both parameters are positive numbers.
}
}

\references{
Johnson N, Kotz S, Kemp A (1992) Univariate Discrete Distributions, John Wiley and Sons, New York

http://www.am.qub.ac.uk/users/g.gribakin/sor/Chap3.pdf

}
\author{S. Nadarajah, B. V. Popovic, M. M. Ristic

}
\examples{
params<-c(4,5)
pgfIneymantypea(.5,params)


##The function is currently defined as

pgfIneymantypea <- function(s,params) {
k<-s[abs(s)>1]
if (length(k)>0)
warning("At least one element of the vector s are out of interval [-1,1]")
if (length(params)<2) 
     stop("At least one value in params is missing")
if (length(params)>2) 
     stop("The length of params is 2")
    theta<-params[1]
    lambda<-params[2]
if (s<=exp(-lambda))
    stop("Logarithm function is not defined. ")
if (theta<=0)
    stop ("Parameter theta must be positive")
if (lambda<=0)
    stop ("Parameter lambda must be positive")
    1+1/theta*log(1+log(s)/lambda)
}
}

