\name{pgfwaring}
\alias{pgfwaring}
\title{
Function pgfwaring
}
\description{
This function calculates value of the pgf of the Waring distribution.
}
\usage{
pgfwaring(s, params)
}
\arguments{
  \item{s}{
Value of the parameter of the pgf. It should be form interval   [-1,1]. In the opposite pgf diverges.
}
  \item{params}{
List of the parameters of the Waring distribution,  such that   params<-c(c,a), where $c>a$ and both parameters are positive.

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
params<-c(3,2)
pgfwaring(.5,params)

## The function is currently defined as

pgfwaring <- function(s,params) {
    require(hypergeo)
k<-s[abs(s)>1]
if (length(k)>0)
    warning("At least one element of the vector s are out of interval [-1,1]")
if (length(params)<2) 
    stop("At least one value in params is missing")
if (length(params)>2) 
    stop("The length of params is 2")
    cc<-params[1]
    a<-params[2]
if (cc<=0)
    stop ("Parameter c must be positive")
if (a<=0)
    stop ("Parameter a must be positive")
(cc-a)/cc*Re(hypergeo(1,a,cc+1,s))
}
}

