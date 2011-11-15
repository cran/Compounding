\name{compoundDist}
\alias{compoundDist}
\docType{data}
\title{
Compound distribution}
\description{
This list is necessary to use functions pCompound, dCompound, qCompound, rCompound, hCompound, momentCompound, meanCompound, varCompound, skewCompound, kurtCompound . This list defines the range of discrete distributions to which the package was defined.
  
}
\usage{data(compoundDist)}
\format{
  The format is:
 chr [1:24] "geometric" "poisson" "binomial" "negativebinomial" "lorarithmic" "binomialbinomial" "binomialpoisson" "poissonbinomial" "neymantypea" "neymantypeb" "neymantypec" "polyaaepli" "poissonpascal" "pascalpoisson" "logarithmicbinomial" "logarithmicpoisson" "poissonlindley" "hyperpoisson" "yule" "waring" "kattitypeh1" "kattitypeh2" "hypergeometric" "thomas" }

\examples{
## This list should be definned as follows
compoundDist <- c("geometric","poisson","negativebinomial","binomial",
"logarithmic","binomialbinomial","binomialpoisson",
"poissonbinomial","neymantypea","polyaaeppli",
"poissonpascal","pascalpoisson",
"logarithmicbinomial","logarithmicpoisson",
"poissonlindley",
"hyperpoisson","yule","waring","kattitypeh1",
"kattitypeh2","neymantypeb","neymantypec",
"hypergeometric","thomas")
}