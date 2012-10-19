\name{momentCompound}
\alias{momentCompound}
%\alias{pgfgeometric}
%\alias{pgfpoisson}
%\alias{pgfbinomial}
%\alias{pgfnegativebinomial}
%\alias{pgflogarithmic}
%\alias{pgfbinomialbinomial}
%\alias{pgfbinomialpoisson}
%\alias{pgfpoissonbinomial}
%\alias{pgfneymantypea}
%\alias{pgfneymantypeb}
%\alias{pgfneymantypec}
%\alias{pgfpolyaaeppli}
%\alias{pgfpoissonpascal}
%\alias{pgfpascalpoisson}
%\alias{pgflogarithmicbinomial}
%\alias{pgflogarithmicpoisson}
%\alias{pgfpoissonlindley}
%\alias{pgfhyperpoisson}
%\alias{pgfyule}
%\alias{pgfwaring}
%\alias{pgfkattitypeh1}
%\alias{pgfkattitypeh2}
%\alias{pgfhypergeometric}
%\alias{pgfthomas}

%\alias{pgfDgeometric}
%\alias{pgfDpoisson}
%\alias{pgfDbinomial}
%\alias{pgfDnegativebinomial}
%\alias{pgfDlogarithmic}
%\alias{pgfDbinomialbinomial}
%\alias{pgfDbinomialpoisson}
%\alias{pgfDpoissonbinomial}
%\alias{pgfDneymantypea}
%\alias{pgfDneymantypeb}
%\alias{pgfDneymantypec}
%\alias{pgfDpolyaaeppli}
%\alias{pgfDpoissonpascal}
%\alias{pgfDpascalpoisson}
%\alias{pgfDlogarithmicbinomial}
%\alias{pgfDlogarithmicpoisson}
%\alias{pgfDpoissonlindley}
%\alias{pgfDhyperpoisson}
%\alias{pgfDyule}
%\alias{pgfDwaring}
%\alias{pgfDkattitypeh1}
%\alias{pgfDkattitypeh2}
%\alias{pgfDhypergeometric}
%\alias{pgfDthomas}

\title{
function momentCompound
}
\description{
Function momentCompound calculates moments of the random variable X.
}
\usage{
momentCompound(k,parent,compound,compoundDist,params,...)
}
\arguments{
    \item{k}{
integer number representing moment's order
}

    \item{parent}{
name of the parent distribution. It can be any continuous  distribution  supported by R.
}
  \item{compound}{
name of the compound distribution. It can be any discrete  distribution supported by this package.


}
\item{compoundDist}{
list of available compounding distributions
}
  \item{params}{
Parameter or list of parameters of compounding distribution. 
}
  \item{\dots}{
Parameters of continuous distribution could be provided as additional parameters. 
}
}
\details{
Parameters of the parent distribution must be provided in the same way as it is in built in R functions. See
http://127.0.0.1:23174/library/stats/html/Distributions.html
}

\references{
S. Nadarajah, B.V. Popovic, M.M. Ristic (2012) Compounding: an R package for computing continuous
distributions obtained by compounding a continuous
and a discrete distribution, Computational Statistics, DOI 10.1007/s00180-012-0336-y, http://www.springerlink.com/content/6r464013w6mp3545/ 
}
\author{

S. Nadarajah, B. V. Popovic, M. M. Ristic
}
\examples{
compoundDist <- c("geometric","poisson","negativebinomial","binomial",
"logarithmic","binomialbinomial","binomialpoisson",
"poissonbinomial","neymantypea","polyaaeppli",
"poissonpascal","pascalpoisson",
"logarithmicbinomial","logarithmicpoisson",
"poissonlindley",
"hyperpoisson","yule","waring","kattitypeh1",
"kattitypeh2","neymantypeb","neymantypec",
"hypergeometric","thomas")
k<-3
parentD<-"beta"
compoundD<-"hypergeometric"
params<-c(3,2,0.5)
momentCompound(k,parentD,compoundD,compoundDist,params,shape1=2,shape2=0.3)

## The function is currently defined as
function(k, parent, compound, compoundDist,params, ...) {
    if (!exists(paste("p",parent,sep=""))) {
        return(paste("The parent distribution",parent,"doesn't exist"))
    }
    if (!is.element(compound,compoundDist)) {
        return(paste("The discrete distribution",compound,"doesn't exist"))
    }
    Finv <- get(paste("q", parent, sep = ""), mode = "function")
    phi <- get(paste("pgf",compound,sep=""), mode = "function")
    phiD <- get(paste("pgfD",compound,sep=""), mode = "function")
    fint <- function(x) phiD(1-x,params)*(Finv(x,...))^k/(1-phi(0,params))
    return(integrate(fint,lower=0,upper=1)$value)
}

}

