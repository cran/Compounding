\name{qCompound}
\alias{qCompound}
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


%\alias{pgfIgeometric}
%\alias{pgfIpoisson}
%\alias{pgfIbinomial}
%\alias{pgfInegativebinomial}
%\alias{pgfIlogarithmic}
%\alias{pgfIbinomialbinomial}
%\alias{pgfIbinomialpoisson}
%\alias{pgfIpoissonbinomial}
%\alias{pgfIneymantypea}
%\alias{pgfIneymantypeb}
%\alias{pgfIneymantypec}
%\alias{pgfIpolyaaeppli}
%\alias{pgfIpoissonpascal}
%\alias{pgfIpascalpoisson}
%\alias{pgfIlogarithmicbinomial}
%\alias{pgfIlogarithmicpoisson}
%\alias{pgfIpoissonlindley}
%\alias{pgfIhyperpoisson}
%\alias{pgfIyule}
%\alias{pgfIwaring}
%\alias{pgfIkattitypeh1}
%\alias{pgfIkattitypeh2}
%\alias{pgfIhypergeometric}
%\alias{pgfIthomas}
\title{
function qCompound
}
\description{
Function qCompound calculates quantiles of the random variable X.
}
\usage{
qCompound(p,parent,compound,compoundDist,params,...)
}
\arguments{
    \item{p}{
vector of probabilities
}
    \item{parent}{
name of the parent distribution. It can be any continuous distribution  supported by R.
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
Nadarajah S, Popovic B.V, Ristic M.M (2011) Compounding: An R Package for Computing Continuous Distributions Obtained by Compounding a Continuous and a Discrete Distribution (submitted)
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
p<-0.5
parentD<-"beta"
compoundD<-"hypergeometric"
params<-c(3,2,0.5)
qCompound(p,parentD,compoundD,params,shape1=2,shape2=0.3)

## The function is currently defined as
qCompound <- function(p,parent,compound,compoundDist,params,...) {
    if (!exists(paste("p",parent,sep=""))) {
        return(paste("The parent distribution",parent,"doesn't exist"))
    }
    if (!is.element(compound,compoundDist)) {
        return(paste("The discrete distribution",compound,"doesn't exist"))
    }

    l<-p[p<0|p>1]
    if (length(l)>0) stop("Parameter p is probability") 

    xval <- real(length(p))
    Finv <- get(paste("q", parent, sep = ""), mode = "function")
    phi <- get(paste("pgf", compound, sep = ""), mode = "function")
    phiInv <- get(paste("pgfI", compound, sep = ""), mode = "function")
    xval <- Finv(1-phiInv(1-p*(1-phi(0,params)),params),...)
    return(xval)
}
}

