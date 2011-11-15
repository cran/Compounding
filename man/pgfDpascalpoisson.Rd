\name{pgfDpascalpoisson}
\alias{pgfDpascalpoisson}
\title{
Function pgfDpascalpoisson
}
\description{
This function calculates value of the pgf's first derivative of the Pascal Poisson distribution.
}
\usage{
pgfDpascalpoisson(s, params)
}
\arguments{
  \item{s}{
Value of the parameter of the pgf. It should be form interval   [-1,1]. In the opposite pgf diverges.
}
  \item{params}{
List of the parameters of the Pascal Poisson distribution, such that   params<-c(theta,mu, a), where 
all parameters are  positive.
}
}

\references{
Johnson N, Kotz S, Kemp A (1992) Univariate Discrete Distributions, John Wiley and Sons, New York

http://www.am.qub.ac.uk/users/g.gribakin/sor/Chap3.pdf

}
\author{S. Nadarajah, B. V. Popovic, M. M. Ristic

}
\examples{
params<-c(3,2,.5)
pgfDpascalpoisson(.5,params)


## The function is currently defined as

pgfDpascalpoisson <- function(s,params) {
k<-s[abs(s)>1]
if (length(k)>0)
     warning("At least one element of the vector s are out of interval [-1,1]")
if (length(params)<3) 
     stop("At least one value in params is missing")
if (length(params)>3) 
     stop("The length of params is 3")
    theta<-params[1]
    mu<-params[2]
    a<-params[3]
if (theta<=0)
    stop ("Parameter theta must be positive")
if (mu<=0)
    stop ("Parameter mu must be positive")
if (a<=0)
    stop ("Parameter a must be positive")
    mu*exp(theta*(s-1))*(1+mu/(a*theta)-mu/(a*theta)*exp(theta*(s-1)))^(-a-1)
}
}

