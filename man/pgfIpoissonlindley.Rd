\name{pgfIpoissonlindley}
\alias{pgfIpoissonlindley}
%\alias{pgfpoissonlindley}
\title{
Function pgfIpoissonlindley
}
\description{
This function calculates value of the pgf's inverse derivative of the Poisson-Lindley distribution.
}
\usage{
pgfIpoissonlindley(s, params)
}
\arguments{
  \item{s}{
Value of the parameter of the pgf. It should be from interval   [-1,1]. In the opposite pgf diverges.
}
  \item{params}{
Positive parameter of the  Poisson-Lindley distribution,  such that   params<-theta.
}
}


\references{
Johnson N, Kotz S, Kemp A (1992) Univariate Discrete Distributions, John Wiley and Sons, New York

http://www.am.qub.ac.uk/users/g.gribakin/sor/Chap3.pdf

}
\author{S. Nadarajah, B. V. Popovic, M. M. Ristic

}

\examples{
params<-5
pgfIpoissonlindley(.9,params)


## The function is currently defined as

pgfIpoissonlindley <- function(s,params) {
    xval<-length(s)
    theta<-params[1]
    for (i in 1:length(s)) {
        func<-function(x) pgfpoissonlindley(x,params)-s[i]
        xval[i]<-uniroot(func,lower=0,upper=1)$root
    }
    xval
}
}

