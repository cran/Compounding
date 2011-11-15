\name{pgfkattitypeh1}
\alias{pgfkattitypeh1}
\title{
Function pgfkattitypeh1
}
\description{
This function calculates value of the pgf of the Katti type H1 distribution.
}
\usage{
pgfkattitypeh1(s, params)
}
\arguments{
  \item{s}{
Value of the parameter of the pgf. It should be from interval   [-1,1]. In the opposite pgf diverges.
}
  \item{params}{
List of the parameters of the Katti type H1 distribution, such that  params<-c(theta,a,b), where all parameter are positive.
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
params<-c(.2,4,.5)
pgfkattitypeh1(.5,params)


## The function is currently defined as

pgfkattitypeh1 <- function(s,params) {
    require(hypergeo)
k<-s[abs(s)>1]
if (length(k)>0)
       warning("At least one element of the vector s are out of interval [-1,1]")
if (length(params)<3) 
       stop("At least one value in params is missing")
if (length(params)>3) 
       stop("The length of params is 3")
    theta<-params[1]
    a<-params[2]
    b<-params[3]
if (theta<=0)
       stop ("Parameter theta must be positive")
if (a<=0)
       stop ("Parameter a must be positive")
if (b<=0)
       stop ("Parameter b must be positive")

    genhypergeo(a,a+b,theta*(s-1))
}
}

