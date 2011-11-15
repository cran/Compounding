\name{pgfIneymantypec}
\alias{pgfIneymantypec}
%\alias{pgfneymantypec}
\title{
Function pgfIneymantypec
}
\description{
This function calculates value of the pgf of the Neyman type C distribution.
}
\usage{
pgfIneymantypec(s, params)
}
\arguments{
  \item{s}{
Value of the parameter of the pgf. It should be from interval   [-1,1]. In the opposite pgf diverges.
}
  \item{params}{
List of the parameters of the Neyman type C distribution, such that params<-c(theta,lambda), where both parameters 
are positive numbers
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
params<-c(4,5)
pgfIneymantypec(.9,params)

## The function is currently defined as

pgfIneymantypec <- function(s,params) {
    xval<-length(s)
    for (i in 1:length(s)) {
        func<-function(x) pgfneymantypec(x,params)-s[i]
        xval[i]<-uniroot(func,lower=0,upper=1)$root
    }
    xval
}
}

