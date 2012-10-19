\name{pgfbinomialbinomial}
\alias{pgfbinomialbinomial}
\title{
Function pgfbinomialbinomial
}
\description{
This function calculates value of the pgf of the binomial-binomial distribution.
}
\usage{
pgfbinomialbinomial(s, params)
}
\arguments{
  \item{s}{
Value of the parameter of the pgf. It should be from interval  [-1,1]. In the opposite pgf diverges.
}
  \item{params}{
List of the parameters of the binomial-binomial distribution,  such that   params<-c(p1,p2,m,n), where
theta is the positive number,
p1, p2 are the probabilities, and 
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

params<-c(.3,.2,4,5)
pgfbinomialbinomial(.5,params)


## The function is currently defined as

pgfbinomialbinomial <- function(s,params) {
k<-s[abs(s)>1]
if (length(k)>0)
warning("At least one element of the vector s are out of interval [-1,1]")
if (length(params)<4) stop("At least one value in params is missing")
if (length(params)>4) stop("The length of params is 4")
    p1<-params[1]
    p2<-params[2]
    m<-params[3]
    n<-params[4]
if ((p1>=1)|(p1<=0))
stop ("Parameter p1 belongs to the interval (0,1)")
if ((p2>=1)|(p2<=0))
stop ("Parameter p2 belongs to the interval (0,1)")
if (m<0)
stop("Parameter m must be positive integer")
if (n<0)
     stop("Parameter n must be positive")
 if(!(abs(n-round(n))<.Machine$double.eps^0.5))
stop("Parameter n must be positive integer")
    (1-p1+p1*(1-p2+p2*s)^n)^m
}
}

