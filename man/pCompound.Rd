\name{pCompound}
\alias{pCompound}
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

\title{
function pCompound
}
\description{
Function pCompound calculates values of the cdf of the random variable X.
}
\usage{
pCompound(q,parent,compound,compoundDist,params,...)
}
\arguments{
    \item{q}{
vector of quantiles
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
q<-0.5
parentD<-"beta"
compoundD<-"hypergeometric"
params<-c(3,2,0.5)
pCompound(q,parentD,compoundD,compoundDist,params,shape1=2,shape2=0.3)

## The function is currently defined as

pCompound <- function(q,parent,compound,compoundDist,params,...) {
    if (!exists(paste("p",parent,sep=""))) {
        return(paste("The parent distribution",parent,"doesn't exist"))
    }
    if (!is.element(compound,compoundDist)) {
        return(paste("The discrete distribution",compound,"doesn't exist"))
    }
    xval <- real(length(q))
    F <- get(paste("p",parent,sep=""), mode = "function")
    phi <- get(paste("pgf",compound,sep=""), mode = "function")
    xval <- (1-phi(1-F(q,...),params))/(1-phi(0,params))
    return(xval)
}
}

