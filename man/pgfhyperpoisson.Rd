\name{pgfhyperpoisson}
\alias{pgfhyperpoisson}
\title{
Function pgfhyperpoisson
}
\description{
This function calculates value of the pgf of the hyper Poisson distribution.
}
\usage{
pgfhyperpoisson(s, params)
}
\arguments{
  \item{s}{
Value of the parameter of the pgf. It should be from interval   [-1,1]. In the opposite pgf diverges.
}
  \item{params}{
List of the parameters of the hyperpoisson distribution, such that   params<-c(theta,lambda), where 
both parameters are  positive.
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
params<-c(.4,.3)
pgfhyperpoisson(.5,params)


## The function is currently defined as

pgfhyperpoisson <- function(s,params) {
    require(hypergeo)
k<-s[abs(s)>1]
if (length(k)>0)
warning("At least one element of the vector s are out of interval [-1,1]")
if (length(params)<2) 
    stop("At least one value in params is missing")
if (length(params)>2) 
    stop("The length of params is 2")

    theta<-params[1]
    lambda<-params[2]
if (theta<=0)
    stop ("Parameter theta must be positive")
if (lambda<=0)
    stop ("Parameter lambda must be positive")
genhypergeo(1,lambda,theta*s)/genhypergeo(1,lambda,theta)
}
}


