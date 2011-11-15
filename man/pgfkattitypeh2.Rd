\name{pgfkattitypeh2}
\alias{pgfkattitypeh2}
\title{
Function pgfkattitypeh2
}
\description{
This function calculates value of the pgf of the Katti type H2 distribution.
}
\usage{
pgfkattitypeh2(s, params)
}
\arguments{
  \item{s}{
Value of the parameter of the pgf. It should be from interval   [-1,1]. In the opposite pgf diverges.
}
  \item{params}{
List of the parameters of the Katti type H2  distribution, such that  params<-c(theta,a,b,k), where all parameter are positive.
}
}

\references{
Johnson N, Kotz S, Kemp A (1992) Univariate Discrete Distributions, John Wiley and Sons, New York

Hankin R.K.S, Lee A (2006) A new family of non-negative distributions. Australia and New Zealand Journal of Statistics 48(1): 67(78)

http://www.am.qub.ac.uk/users/g.gribakin/sor/Chap3.pdf

}
\author{S. Nadarajah, B. V. Popovic, M. M. Ristic

}

\examples{
params<-c(.2,4,.5,.3)
pgfkattitypeh2(.5,params)


## The function is currently defined as

pgfkattitypeh2 <- function(s,params) {
    require(hypergeo)
k<-s[abs(s)>1]
if (length(k)>0)
      warning("At least one element of the vector s are out of interval [-1,1]")

if (length(params)<4) 
      stop("At least one value in params is missing")
if (length(params)>4) 
      stop("The length of params is 4")
    theta<-params[1]
    a<-params[2]
    b<-params[3]
    k<-params[4]
if (theta<=0)
     stop ("Parameter theta must be positive")
if (a<=0)
     stop ("Parameter a must be positive")
if (b<=0)
     stop ("Parameter b must be positive")
if (k<=0)
     stop ("Parameter k must be positive")

    Re(hypergeo(k,a,a+b,theta*(s-1)))
}
}

