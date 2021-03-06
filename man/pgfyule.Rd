\name{pgfyule}
\alias{pgfyule}

\title{
Function pgfyule
}
\description{
This function calculates value of the pgf of the Yule distribution.
}
\usage{
pgfyule(s, params)
}
\arguments{
  \item{s}{
Value of the parameter of the pgf. It should be from interval   [-1,1]. In the opposite pgf divegrates.
}
  \item{params}{
Postive parameter of the  Yule distribution,  such that   params<-theta.
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
params<-3
pgfyule(.5,params)

## The function is currently defined as

pgfyule <- function(s,params) {
    require(hypergeo)
k<-s[abs(s)>1]
if (length(k)>0)
    warning("At least one element of the vector s are out of interval [-1,1]")
if (length(params)>1) 
    stop("The length of params is 1")
    theta<-params[1]
if (theta<=0)
    stop ("Parameter theta must be positive")
theta/(theta+1)*Re(hypergeo(1,1,theta+2,s))
}
}
