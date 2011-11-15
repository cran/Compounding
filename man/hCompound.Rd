\name{hCompound}
\alias{hCompound}
%\alias{dCompound}
%\alias{pCompound}
\title{
function hCompound
}
\description{
Function hCompound calculates value of the hazard rate function of the random variable X.
}
\usage{
hCompound(x,parent,compound,compoundDist,params,...)
}
\arguments{
  \item{x}{
vector of quantiles
}
  \item{parent}{
name of the parent distribution. It can be any  continuous  distribution  supported by R.

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
Parameters of continuous  distribution could be provided as additional parameters. 
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
q<-0.5
parentD<-"beta"
compoundD<-"hypergeometric"
params<-c(3,2,0.5)
hCompound(q,parentD,compoundD,compoundDist,params,shape1=2,shape2=0.3)

## The function is currently defined as
hCompound <- function(x, parent, compound,params, ...) {
    if (!exists(paste("p",parent,sep=""))) {
        return(paste("The parent distribution",parent,"doesn't exist"))
    }
    if (!is.element(compound,compoundDist)) {
        return(paste("The discrete distribution",compound,"doesn't exist"))
    }
    dCompound(x,parent,compound,compoundDist,params,...)/(1-pCompound(x,parent,compound,compoundDist,params,...))
}

}

