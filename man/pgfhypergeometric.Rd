\name{pgfhypergeometric}
\alias{pgfhypergeometric}

\title{
Function pgfhypergeometric
}
\description{
This function calculates value of the pgf of the hypergeometric distribution.
}
\usage{
pgfhypergeometric(s, params)
}
\arguments{
  \item{s}{
Value of the parameter of the pgf. It should be from interval   [-1,1]. In the opposite pgf diverges.
}
  \item{params}{
List of the parameters of the hypergeometric distribution, such that  params<-c(m,n,p) where
m is the number of white balls in the urn,
n is the number of black balls in the urn, must be less or equal than m, and
p is probability.
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
params<-c(5,3,.5)
pgfhypergeometric(.5,params)


## The function is currently defined as

pgfhypergeometric <- function(s,params) {
    require(hypergeo)
k<-s[abs(s)>1]
if (length(k)>0)
warning("At least one element of the vector s are out of interval [-1,1]")
if (length(params)<3) 
    stop("At least one value in params is missing")
if (length(params)>3) 
    stop("The length of params is 3")
    m<-params[1]
    n<-params[2]
    p<-params[3]
if (n<0)
     stop("Parameter n must be positive")
 if(!(abs(n-round(n))<.Machine$double.eps^0.5))
stop("Parameter n must be positive integer")
if (m<0)
     stop("Parameter m must be positive")
 if(!(abs(m-round(m))<.Machine$double.eps^0.5))
stop("Parameter m must be positive integer")
if ((p>=1)|(p<=0))
   stop ("Parameter p belongs to the interval (0,1)")
if (m<n)
   stop ("Parameter m is greater or equal then n ")
    Re(hypergeo(-n,-m*p,-m,1-s))
}

}



