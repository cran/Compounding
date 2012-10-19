\name{pgfDlogarithmicbinomial}
\alias{pgfDlogarithmicbinomial}
\title{
Function pgfDlogarithmicbinomial.
}
\description{
This function calculates value of the pgf's first derivative of the logarithmic-binomial distribution.
}
\usage{
pgfDlogarithmicbinomial(s, params)
}
\arguments{
  \item{s}{
Value of the parameter of the pgf. It should be from interval   [-1,1]. In the opposite pgf diverges.
}
  \item{params}{
List of the parameters of the logarithmic-binomial  distribution, such that  params<-c(p1,p2,m,n), where 
p1, p2 are the probabilities,
m, n are the positive integers.
}
}

\references{
Johnson N, Kotz S, Kemp A (1992) Univariate Discrete Distributions, John Wiley and Sons, New York

http://www.am.qub.ac.uk/users/g.gribakin/sor/Chap3.pdf

}
\author{S. Nadarajah, B. V. Popovic, M. M. Ristic

}
\examples{
params<-c(.9,.7,4)
pgfDlogarithmicbinomial(.5,params)

## The function is currently defined as

pgfDlogarithmicbinomial <- function(s,params) {
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
if ((theta>=1)|(theta<=0))
    stop ("Parameter theta belongs to the interval (0,1)")
if ((p>=1)|(p<=0))
    stop ("Parameter p belongs to the interval (0,1)")
if (n<0)
     stop("Parameter n must be positive")
 if(!(abs(n-round(n))<.Machine$double.eps^0.5))
stop("Parameter n must be positive integer")
-n*p*(1-theta)/log(theta)*(1-p+p*s)^(n-1)*(1-(1-theta)*(1-p+p*s)^n)^(-1)
}
}

