\name{Compounding-package}
\alias{Compounding-package}
%\alias{Compounding}
%\alias{dCompound}
%\alias{hCompound}
%\alias{kurtCompound}
%\alias{meanCompound}
%\alias{momentCompound}
%\alias{pCompound}

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

\docType{package}
\title{
Calculation of the main characteristics of compounding distribution. 
}
\description{
Package Compounding provides values of the pdf, cdf and hazard rate functions of the compounding distribution. Also it is possible to draw random sample and to compute main characteristics of the compounding distribution.

Compound distributions can be characterized as follows:

Suppose a device has an unknown number, N,  of initial defects of same kind (for example, a number of semiconductors from a defective lot).
Suppose X_i's represent their lifetimes and that  each defect can be detected only after causing failure.
Then the time to the first failure of the device is X = min(X_1, X_2, ldots, X_N).


Suppose a parallel system has N components.
Let X_1, X_2, ldots, X_N denote their lifetimes.
The system will fail as soon  as any one of the components fails.
The system's lifetime is X = min (X_1, X_2, ldots, X_N).}

\details{
\tabular{ll}{
Package: \tab Compounding\cr
Type: \tab Package\cr
Version: \tab 1.0.1\cr
Date: \tab 2012-10-19\cr

License: \tab What license is it under?\cr
LazyLoad: \tab yes\cr
}
In this package we give some  programs for working with continuous distributions obtained
by compounding continuous distributions with discrete distributions.
The programs compute values of cumulative distribution function, probability density function,
quantile function and hazard rate function, generate random samples from a population with
compounding distribution, and compute mean, variance, skewness and kurtosis of a random variable with a compounding distribution.
We consider 24 discrete distributions which can be compounded with any continuous distribution implemented in R.
}
\author{ S.Nadarajah, B. V. Popovic, M. M. Ristic,


 Maintainer: B.V. Popovic 
 Email: bozidarpopovic@gmail.com 
}
\references{
S. Nadarajah, B.V. Popovic, M.M. Ristic (2012) Compounding: an R package for computing continuous
distributions obtained by compounding a continuous
and a discrete distribution, Computational Statistics, DOI 10.1007/s00180-012-0336-y, http://www.springerlink.com/content/6r464013w6mp3545/ 
}

\keyword{ package }

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
parentD<-"exp"
compoundD<-"poisson"
params<-2.5
x<-0.5
k<-2
dCompound(x,parentD,compoundD,compoundDist,params)
qCompound(x,parentD,compoundD,compoundDist,params)
hCompound(x,parentD,compoundD,compoundDist,params)
momentCompound(k, parentD, compoundD, compoundDist,params)
}

