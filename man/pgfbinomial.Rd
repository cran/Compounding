\name{pgfbinomial}
\alias{pgfbinomial}
\title{
Function pgfbinomial
}
\description{
This function calculates value of the pgf of the binomial distribution.
}
\usage{
pgfbinomial(s, params)
}
\arguments{
  \item{s}{
Value of the parameter of the pgf. It should be from interval   [-1,1]. In the opposite pgf diverges.
}
  \item{params}{
List of the parameters of the binomial distribution, such that   params<-c(n,theta), where n is the size, while 
theta is the probability.
}
}


\references{
Johnson N, Kotz S, Kemp A (1992) Univariate Discrete Distributions, John Wiley and Sons, New York

http://www.am.qub.ac.uk/users/g.gribakin/sor/Chap3.pdf

}
\author{S. Nadarajah, B. V. Popovic, M. M. Ristic

}
\examples{
params<-c(3,.2)
pgfbinomial(.5,params)


##The function is currently defined as

pgfbinomial <- function(s,params) {
k<-s[abs(s)>1]
if (length(k)>0)
    warning("At least one element of the vector s are out of interval [-1,1]")
if (length(params)<2) 
    stop("At least one value in params is missing")
if (length(params)>2) 
    stop("The length of params is 2")
       n<-params[1]
       theta<-params[2]
if ((theta>=1)|(theta<=0))
    stop ("Parameter theta belongs to the interval (0,1)")
if (n<0)
     stop("Parameter n must be positive")
 if(!(abs(n-round(n))<.Machine$double.eps^0.5))
stop("Parameter n must be positive integer")
    (1-theta+theta*s)^n
}
}

