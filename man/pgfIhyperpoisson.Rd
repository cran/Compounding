\name{pgfIhyperpoisson}
\alias{pgfIhyperpoisson}
%\alias{pgfhyperpoisson}
\title{
Function pgfIhyperpoisson
}
\description{
This function calculates value of the pgf's inverse of the hyper Poisson distribution.
}
\usage{
pgfIhyperpoisson(s, params)
}
\arguments{
  \item{s}{
Value of the parameter of the pgf. It should be from interval   [-1,1]. In the opposite pgf diverges.
}
  \item{params}{
List of the parameters of the hyperpoisson distribution, such that  params<-c(theta,lambda), where 
both  parameters are positive.
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
params<-c(.7,3)
pgfIhyperpoisson(.9,params)

##The function is currently defined as

pgfIhyperpoisson <- function(s,params) {
    xval<-length(s)
    for (i in 1:length(s)) {
        func<-function(x) pgfhyperpoisson(x,params)-s[i]
        xval[i]<-uniroot(func,lower=0,upper=1)$root
    }
    xval
}
}

