\name{pgfIthomas}
\alias{pgfIthomas}
%\alias{pgfthomas}
\title{
Function pgfIthomas
}
\description{
This function calculates value of the pgf's inverse of the Thomas distribution.
}
\usage{
pgfIthomas(s, params)
}
\arguments{
  \item{s}{
Value of the parameter of the pgf. It should be from interval   [-1,1]. In the opposite pgf diverges.
}
  \item{params}{
List of the parameters of the Thomas distribution,  such that   params<-c(lambda,theta), where
both parameters are  positive.

}
}

\references{
Johnson N, Kotz S, Kemp A (1992) Univariate Discrete Distributions, John Wiley and Sons, New York

http://www.am.qub.ac.uk/users/g.gribakin/sor/Chap3.pdf

}
\author{S. Nadarajah, B. V. Popovic, M. M. Ristic

}

\examples{
params<-c(5,.4)
pgfIthomas(.9,params)


## The function is currently defined as

pgfIthomas <- function(s,params) {
    xval<-length(s)
    for (i in 1:length(s)) {
        func<-function(x) pgfthomas(x,params)-s[i]
        xval[i]<-uniroot(func,lower=0,upper=1)$root
    }
    print(xval)
    xval
}

}

