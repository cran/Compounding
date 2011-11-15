compoundDist <- c("geometric","poisson","negativebinomial","binomial",
                  "logarithmic","binomialbinomial","binomialpoisson",
                  "poissonbinomial","neymantypea","polyaaeppli",
                  "poissonpascal","pascalpoisson",
                  "logarithmicbinomial","logarithmicpoisson",
                  "poissonlindley",
                  "hyperpoisson","yule","waring","kattitypeh1",
                  "kattitypeh2","neymantypeb","neymantypec",
                  "hypergeometric","thomas")

# Geometric distribution

pgfgeometric <- function(s,params) {
k<-s[abs(s)>1]

if (length(k)>0)
warning("At least one element of the vector s are out of interval [-1,1]")


if (length(params)>1) stop("The length of params is 1")
 theta<-params[1]


if ((theta>=1)|(theta<=0))

stop ("Parameter of geometric distribution must belong  to the interval (0,1)")

    theta/(1-(1-theta)*s)
}


pgfDgeometric <- function(s,params) {
k<-s[abs(s)>1]


if (length(k)>0)
warning("At least one element of the vector s are out of interval [-1,1]")


if (length(params)>1) stop("The length of params is 1")

    theta<-params[1]
if ((theta>=1)|(theta<=0))
stop ("Parameter of geometric distribution must belong  to the interval (0,1)")
    theta*(1-theta)/(1-(1-theta)*s)^2
}



pgfIgeometric <- function(s,params) {
k<-s[abs(s)>1]


if (length(k)>0)
warning("At least one element of the vector s are out of interval [-1,1]")


if (length(params)>1) stop("The length of params is 1")



    theta<-params[1]
if ((theta>=1)|(theta<=0))
stop ("Parameter of geometric distribution must belong  to the interval (0,1)")
    (s-theta)/((1-theta)*s)
}



# Poisson distribution

pgfpoisson <- function(s,params) {
k<-s[abs(s)>1]


if (length(k)>0)
warning("At least one element of the vector s are out of interval [-1,1]")
if (missing(params)) stop("Distribution parameters are not defined")
    theta<-params[1]
if (theta<=0)
stop ("Parameter of Poisson distribution must be positive")
    exp(theta*(s-1))
}

pgfDpoisson <- function(s,params) {
   k<-s[abs(s)>1]
if (length(k)>0)
    warning("At least one element of the vector s are out of interval [-1,1]")
if (length(params)>1) stop("The length of params is 1")
    theta<-params[1]
if (theta<=0) 
    stop ("Parameter of Poisson distribution must be positive")
    theta*exp(theta*(s-1))
}



pgfIpoisson <- function(s,params) {
k<-s[abs(s)>1]
if (length(k)>0)
    warning("At least one element of the vector s are out of interval [-1,1]")
 if (length(params)>1) stop("The length of params is 1")
    theta<-params[1]
if (theta<=0) 
    stop ("Parameter of Poisson distribution must be positive")
    1+log(s)/theta
}



# Binomial distribution

pgfbinomial <- function(s,params) {

k<-s[abs(s)>1]


if (length(k)>0)
warning("At least one element of the vector s are out of interval [-1,1]")

if (length(params)<2) stop("At least one value in params is missing")
if (length(params)>2) stop("The length of params is 2")

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


pgfDbinomial <- function(s,params) {

k<-s[abs(s)>1]


if (length(k)>0)

warning("At least one element of the vector s are out of interval [-1,1]")
if (length(params)<2) stop("At least one value in params is missing")
if (length(params)>2) stop("The length of params is 2")

       n<-params[1]
       theta<-params[2]

if ((theta>=1)|(theta<=0))
stop ("Parameter theta belongs to the interval (0,1)")
if (n<0)
     stop("Parameter n must be positive")
 if(!(abs(n-round(n))<.Machine$double.eps^0.5))
stop("Parameter n must be positive integer")
    n*theta*(1-theta+theta*s)^(n-1)
}


pgfIbinomial <- function(s,params) {

k<-s[abs(s)>1]


if (length(k)>0)

warning("At least one element of the vector s are out of interval [-1,1]")
if (length(params)<2) stop("At least one value in params is missing")
if (length(params)>2) stop("The length of params is 2")

       n<-params[1]
       theta<-params[2]

if ((theta>=1)|(theta<=0))
stop ("Parameter theta belongs to the interval (0,1)")
if (n<0)
     stop("Parameter n must be positive")
 if(!(abs(n-round(n))<.Machine$double.eps^0.5))
stop("Parameter n must be positive integer")
    (s^(1/n)-1+theta)/theta
}



# Negative binomial distribution


pgfnegativebinomial <- function(s,params) {
k<-s[abs(s)>1]


if (length(k)>0)
warning("Some  elements of the vector s are out of interval [-1,1]")

if (length(params)<2) stop("At least one value in params is missing")
if (length(params)>2) stop("The length of params is 2")
    theta<-params[1]
    k<-params[2]
if ((theta>=1)|(theta<=0))
stop ("Parameter theta belongs to the interval (0,1)")
if (k<=0)
stop("Parameter k must be positive")
    (theta/(1-(1-theta)*s))^k
}


pgfDnegativebinomial <- function(s,params) {
k<-s[abs(s)>1]


if (length(k)>0)
warning("Some  elements of the vector s are out of interval [-1,1]")

if (length(params)<2) stop("At least one value in params is missing")
if (length(params)>2) stop("The length of params is 2")
    theta<-params[1]
    k<-params[2]
if ((theta>=1)|(theta<=0))
stop ("Parameter theta belongs to the interval (0,1)")
if (k<=0)
stop("Parameter k must be positive")
    k*(1-theta)*theta^k/(1-(1-theta)*s)^(k+1)
}


pgfInegativebinomial <- function(s,params) {
k<-s[abs(s)>1]


if (length(k)>0)
warning("At least one element of the vector s are out of interval [-1,1]")

if (length(params)<2) stop("At least one value in params is missing")
if (length(params)>2) stop("The length of params is 2")
    theta<-params[1]
    k<-params[2]
if ((theta>=1)|(theta<=0))
stop ("Parameter theta belongs to the interval (0,1)")
if (k<=0)
stop("Parameter k must be positive")
    (1-theta*s^(-1/k))/(1-theta)
}

# Logarithmic distribution


pgflogarithmic <- function(s,params) {

k<-s[abs(s)>1]


if (length(k)>0)
warning("At least one element of the vector s are out of interval [-1,1]")

if (length(params)>1) stop("The length of params is 1")

    theta<-params[1]
if ((theta>=1)|(theta<=0))
stop ("Parameter theta belongs to the interval (0,1)")
    log(1-(1-theta)*s)/log(theta)
}

pgfDlogarithmic <- function(s,params) {
k<-s[abs(s)>1]


if (length(k)>0)
warning("At least one element of the vector s are out of interval [-1,1]")

if (length(params)>1) stop("The length of params is 1")
    theta<-params[1]
if ((theta>=1)|(theta<=0))
stop ("Parameter theta belongs to the interval (0,1)")
    -(1-theta)/((1-(1-theta)*s)*log(theta))
}


pgfIlogarithmic <- function(s,params) {
k<-s[abs(s)>1]


if (length(k)>0)
warning("At least one element of the vector s are out of interval [-1,1]")

if (length(params)>1) stop("The length of params is 1")
    theta<-params[1]
if ((theta>=1)|(theta<=0))
stop ("Parameter theta belongs to the interval (0,1)")
    (1-theta^s)/(1-theta)
}

# Binomial-Binomial distribution


pgfbinomialbinomial <- function(s,params) {
k<-s[abs(s)>1]


if (length(k)>0)
warning("At least one element of the vector s are out of interval [-1,1]")

if (length(params)<4) stop("At least one value in params is missing")
if (length(params)>4) stop("The length of params is 4")


    p1<-params[1]
    p2<-params[2]
    m<-params[3]
    n<-params[4]
if ((p1>=1)|(p1<=0))
stop ("Parameter p1 belongs to the interval (0,1)")
if ((p2>=1)|(p2<=0))
stop ("Parameter p2 belongs to the interval (0,1)")
if (m<0)
     stop("Parameter m must be positive")
 if(!(abs(m-round(m))<.Machine$double.eps^0.5))
stop("Parameter m must be positive")
if (n<0)
     stop("Parameter n must be positive")
 if(!(abs(n-round(n))<.Machine$double.eps^0.5))
stop("Parameter n must be positive integer")
    (1-p1+p1*(1-p2+p2*s)^n)^m
}


pgfDbinomialbinomial <- function(s,params) {
k<-s[abs(s)>1]


if (length(k)>0)
warning("At least one element of the vector s are out of interval [-1,1]")

if (length(params)<4) stop("At least one value in params is missing")
if (length(params)>4) stop("The length of params is 4")

    p1<-params[1]
    p2<-params[2]
    m<-params[3]
    n<-params[4]
if ((p1>=1)|(p1<=0))
stop ("Parameter p1 belongs to the interval (0,1)")
if ((p2>=1)|(p2<=0))
stop ("Parameter p2 belongs to the interval (0,1)")
if (m<0)
     stop("Parameter m must be positive")
 if(!(abs(m-round(m))<.Machine$double.eps^0.5))
stop("Parameter m must be positive")
if (n<0)
     stop("Parameter n must be positive")
 if(!(abs(n-round(n))<.Machine$double.eps^0.5))
stop("Parameter n must be positive integer")

    m*n*p1*p2*(1-p2+p2*s)^(n-1)*(1-p1+p1*(1-p2+p2*s)^n)^(m-1)
}


pgfIbinomialbinomial <- function(s,params) {
k<-s[abs(s)>1]


if (length(k)>0)
warning("At least one element of the vector s are out of interval [-1,1]")

if (length(params)<4) stop("At least one value in params is missing")
if (length(params)>4) stop("The length of params is 4")

    p1<-params[1]
    p2<-params[2]
    m<-params[3]
    n<-params[4]
if ((p1>=1)|(p1<=0))
stop ("Parameter p1 belongs to the interval (0,1)")
if ((p2>=1)|(p2<=0))
stop ("Parameter p2 belongs to the interval (0,1)")
if (m<0)
     stop("Parameter m must be positive")
 if(!(abs(m-round(m))<.Machine$double.eps^0.5))
stop("Parameter m must be positive")
if (n<0)
     stop("Parameter n must be positive")
 if(!(abs(n-round(n))<.Machine$double.eps^0.5))
stop("Parameter n must be positive integer")

    zval<-(s^(1/m)-1+p1)/p1
    (zval^(1/n)-1+p2)/p2
}

# Binomial-Poisson distribution
pgfbinomialpoisson <- function(s,params) {
k<-s[abs(s)>1]


if (length(k)>0)

warning("At least one element of the vector s are out of interval [-1,1]")

if (length(params)<3) stop("At least one value in params is missing")
if (length(params)>3) stop("The length of params is 3")
    theta<-params[1]
    p<-params[2]
    n<-params[3]
    


if (theta<=0)
stop ("Parameter theta must be positive")
if ((p>=1)|(p<=0))
stop ("Parameter p belongs to the interval (0,1)")
if (n<0)
     stop("Parameter n must be positive")
 if(!(abs(n-round(n))<.Machine$double.eps^0.5))
stop("Parameter n must be positive integer")

   (1-p+p*exp(theta*(s-1)))^n
}




pgfDbinomialpoisson <- function(s,params) {
k<-s[abs(s)>1]


if (length(k)>0)

warning("At least one element of the vector s are out of interval [-1,1]")

if (length(params)<3) stop("At least one value in params is missing")
if (length(params)>3) stop("The length of params is 3")

    theta<-params[1]
    p<-params[2]
    n<-params[3]
  


if (theta<=0)
stop ("Parameter theta must be positive")
if ((p>=1)|(p<=0))
stop ("Parameter p belongs to the interval (0,1)")
if (n<0)
     stop("Parameter n must be positive")
 if(!(abs(n-round(n))<.Machine$double.eps^0.5))
stop("Parameter n must be positive integer")
    n*theta*p*exp(theta*(s-1))*(1-p+p*exp(theta*(s-1)))^(n-1)
}





pgfIbinomialpoisson <- function(s,params) {

k<-s[abs(s)>1]


if (length(k)>0)

warning("At least one element of the vector s are out of interval [-1,1]")

if (length(params)<3) stop("At least one value in params is missing")
if (length(params)>3) stop("The length of params is 3")
    theta<-params[1]
    p<-params[2]
    n<-params[3]
  


if (theta<=0)
stop ("Parameter theta must be positive")
if ((p>=1)|(p<=0))
stop ("Parameter p belongs to the interval (0,1)")
if (n<0)
     stop("Parameter n must be positive")
 if(!(abs(n-round(n))<.Machine$double.eps^0.5))
stop("Parameter n must be positive integer")
    zval<-(s^(1/n)-1+p)/p
    1+log(zval)/theta
}








# Poisson-Binomial distribution
pgfpoissonbinomial <- function(s,params) {
k<-s[abs(s)>1]


if (length(k)>0)

warning("At least one element of the vector s are out of interval [-1,1]")
if (length(params)<3) stop("At least one value in params is missing")
if (length(params)>3) stop("The length of params is 3")
    theta<-params[1]
    p<-params[2]
    n<-params[3]
  
if (theta<=0)
stop ("Parameter theta must be positive")
if ((p>=1)|(p<=0))
stop ("Parameter p belongs to the interval (0,1)")
if (n<0)
     stop("Parameter n must be positive")
 if(!(abs(n-round(n))<.Machine$double.eps^0.5))
stop("Parameter n must be positive integer")
    exp(theta*((1-p+p*s)^n-1))
}



pgfDpoissonbinomial <- function(s,params) {
k<-s[abs(s)>1]


if (length(k)>0)

warning("At least one element of the vector s are out of interval [-1,1]")
if (length(params)<3) stop("At least one value in params is missing")
if (length(params)>3) stop("The length of params is 3")
    theta<-params[1]
    p<-params[2]
    n<-params[3]
  
if (theta<=0)
stop ("Parameter theta must be positive")
if ((p>=1)|(p<=0))
stop ("Parameter p belongs to the interval (0,1)")
if (n<0)
     stop("Parameter n must be positive")
 if(!(abs(n-round(n))<.Machine$double.eps^0.5))
stop("Parameter n must be positive integer")
    n*theta*p*(1-p+p*s)^(n-1)*exp(theta*((1-p+p*s)^n-1))
}




pgfIpoissonbinomial <- function(s,params) {
k<-s[abs(s)>1]


if (length(k)>0)

warning("At least one element of the vector s are out of interval [-1,1]")
if (length(params)<3) stop("At least one value in params is missing")
if (length(params)>3) stop("The length of params is 3")
    theta<-params[1]
    p<-params[2]
    n<-params[3]
  
if (theta<=0)
stop ("Parameter theta must be positive")
if ((p>=1)|(p<=0))
stop ("Parameter p belongs to the interval (0,1)")
if (n<0)
     stop("Parameter n must be positive")
 if(!(abs(n-round(n))<.Machine$double.eps^0.5))
stop("Parameter n must be positive integer")
    zval<-1+log(s)/theta
    (zval^(1/n)-1+p)/p
}








# Neyman type A distribution

pgfneymantypea<-function(s,params){
k<-s[abs(s)>1]


if (length(k)>0)

warning("At least one element of the vector s are out of interval [-1,1]")

if (length(params)<2) stop("At least one value in params is missing")
if (length(params)>2) stop("The length of params is 2")
    theta<-params[1]
    lambda<-params[2]
if (theta<=0)
stop ("Parameter theta must be positive")
if (lambda<=0)
stop ("Parameter lambda must be positive")

    exp(lambda*(exp(theta*(s-1))-1))
}


pgfDneymantypea <- function(s,params) {
k<-s[abs(s)>1]


if (length(k)>0)

warning("At least one element of the vector s are out of interval [-1,1]")

if (length(params)<2) stop("At least one value in params is missing")
if (length(params)>2) stop("The length of params is 2")
    theta<-params[1]
    lambda<-params[2]
if (theta<=0)
stop ("Parameter theta must be positive")
if (lambda<=0)
stop ("Parameter lambda must be positive")
    lambda*theta*exp(theta*(s-1))*exp(lambda*(exp(theta*(s-1))-1))
}


pgfIneymantypea <- function(s,params) {

k<-s[abs(s)>1]


if (length(k)>0)

warning("At least one element of the vector s are out of interval [-1,1]")

if (length(params)<2) stop("At least one value in params is missing")
if (length(params)>2) stop("The length of params is 2")
    
    theta<-params[1]
    lambda<-params[2]
if (s<=exp(-lambda))
stop("Logarithm function is not defined. ")
if (theta<=0)
stop ("Parameter theta must be positive")
if (lambda<=0)
stop ("Parameter lambda must be positive")
    1+1/theta*log(1+log(s)/lambda)
}

# Neyman type B distribution
pgfneymantypeb <- function(s,params) {
    require(hypergeo)
k<-s[abs(s)>1]


if (length(k)>0)

warning("At least one element of the vector s are out of interval [-1,1]")

if (length(params)<2) stop("At least one value in params is missing")
if (length(params)>2) stop("The length of params is 2")

    theta<-params[1]
    lambda<-params[2]
if (theta<=0)
stop ("Parameter theta must be positive")
if (lambda<=0)
stop ("Parameter lambda must be positive")

    exp(lambda*(genhypergeo(1,2,theta*(s-1))-1))
}

pgfDneymantypeb <- function(s,params) {
    require(hypergeo)
k<-s[abs(s)>1]


if (length(k)>0)

warning("At least one element of the vector s are out of interval [-1,1]")

if (length(params)<2) stop("At least one value in params is missing")
if (length(params)>2) stop("The length of params is 2")
    theta<-params[1]
    lambda<-params[2]
if (theta<=0)
stop ("Parameter theta must be positive")
if (lambda<=0)
stop ("Parameter lambda must be positive")
    theta*lambda/2*genhypergeo(2,3,theta*(s-1))*pgfneymantypeb(s,params)
}


pgfIneymantypeb <- function(s,params) {
    xval<-length(s)
    for (i in 1:length(s)) {
        func<-function(x) pgfneymantypeb(x,params)-s[i]
        xval[i]<-uniroot(func,lower=0,upper=1)$root
    }
    xval
}

# Neyman type C distribution
pgfneymantypec <- function(s,params) {
    require(hypergeo)
k<-s[abs(s)>1]


if (length(k)>0)

warning("At least one element of the vector s are out of interval [-1,1]")

if (length(params)<2) stop("At least one value in params is missing")
if (length(params)>2) stop("The length of params is 2")
    theta<-params[1]
    lambda<-params[2]
if (theta<=0)
stop ("Parameter theta must be positive")
if (lambda<=0)
stop ("Parameter lambda must be positive")
    exp(lambda*(genhypergeo(1,3,theta*(s-1))-1))
}


pgfDneymantypec <- function(s,params) {
    require(hypergeo)
     
k<-s[abs(s)>1]


if (length(k)>0)

warning("At least one element of the vector s are out of interval [-1,1]")

if (length(params)<2) stop("At least one value in params is missing")
if (length(params)>2) stop("The length of params is 2")

    theta<-params[1]
    lambda<-params[2]
if (theta<=0)
stop ("Parameter theta must be positive")
if (lambda<=0)
stop ("Parameter lambda must be positive")
    theta*lambda/3*genhypergeo(2,4,theta*(s-1))*pgfneymantypec(s,params)
}


pgfIneymantypec <- function(s,params) {
    xval<-length(s)
    for (i in 1:length(s)) {
        func<-function(x) pgfneymantypec(x,params)-s[i]
        xval[i]<-uniroot(func,lower=0,upper=1)$root
    }
    xval
}

# Polya-Aeppli distribution


pgfpolyaaeppli <- function(s,params) {
k<-s[abs(s)>1]


if (length(k)>0)

warning("At least one element of the vector s are out of interval [-1,1]")

if (length(params)<2) stop("At least one value in params is missing")
if (length(params)>2) stop("The length of params is 2")
    
theta<-params[1]
    p<-params[2]
if (theta<=0)
stop ("Parameter theta must be positive")

if ((p>=1)|(p<=0))
stop ("Parameter p belongs to the interval (0,1)")

    exp(theta/p*((1-p)/(1-p*s)-1))
}


pgfDpolyaaeppli <- function(s,params) {
k<-s[abs(s)>1]


if (length(k)>0)

warning("At least one element of the vector s are out of interval [-1,1]")

if (length(params)<2) stop("At least one value in params is missing")
if (length(params)>2) stop("The length of params is 2")
    
theta<-params[1]
    p<-params[2]
if (theta<=0)
stop ("Parameter theta must be positive")

if ((p>=1)|(p<=0))
stop ("Parameter p belongs to the interval (0,1)")
    theta*(1-p)/(1-p*s)^2*exp(theta/p*((1-p)/(1-p*s)-1))
}

pgfIpolyaaeppli <- function(s,params) {
k<-s[abs(s)>1]


if (length(k)>0)

warning("At least one element of the vector s are out of interval [-1,1]")

if (length(params)<2) stop("At least one value in params is missing")
if (length(params)>2) stop("The length of params is 2")
    
theta<-params[1]
    p<-params[2]
if (theta<=0)
stop ("Parameter theta must be positive")

if ((p>=1)|(p<=0))
stop ("Parameter p belongs to the interval (0,1)")

    (theta+log(s))/(theta+p*log(s))
}

# Poisson-Pascal distribution

pgfpoissonpascal <- function(s,params) {
m<-s[abs(s)>1]


if (length(m)>0)

warning("At least one element of the vector s are out of interval [-1,1]")

if (length(params)<3) stop("At least one value in params is missing")
if (length(params)>3) stop("The length of params is 3")
    theta<-params[1]
    p<-params[2]
    k<-params[3]
if (theta<=0)
stop ("Parameter theta must be positive")
if (p<=0)
stop ("Parameter lambda must be positive")
if (k<=0)
stop ("Parameter k must be positive")

    exp(theta*((1+p-p*s)^(-k)-1))
}


pgfDpoissonpascal <- function(s,params) {
m<-s[abs(s)>1]


if (length(m)>0)

warning("At least one element of the vector s are out of interval [-1,1]")

if (length(params)<3) stop("At least one value in params is missing")
if (length(params)>3) stop("The length of params is 3")
    theta<-params[1]
    p<-params[2]
    k<-params[3]
if (theta<=0)
stop ("Parameter theta must be positive")
if (p<=0)
stop ("Parameter lambda must be positive")
if (k<=0)
stop ("Parameter k must be positive")
    theta*k*p*(1+p-p*s)^(-k-1)*exp(theta*((1+p-p*s)^(-k)-1))
}



pgfIpoissonpascal <- function(s,params) {
m<-s[abs(s)>1]


if (length(m)>0)

warning("At least one element of the vector s are out of interval [-1,1]")
     

if (length(params)<3) stop("At least one value in params is missing")
if (length(params)>3) stop("The length of params is 3")
    theta<-params[1]
    p<-params[2]
    k<-params[3]
if (theta<=0)
stop ("Parameter theta must be positive")
if (p<=0)
stop ("Parameter lambda must be positive")
if (k<=0)
stop ("Parameter k must be positive")
    (1+p-(1+log(s)/theta)^(-1/k))/p
}






# Pascal-Poisson distribution

pgfpascalpoisson <- function(s,params) {
    
k<-s[abs(s)>1]


if (length(k)>0)

warning("At least one element of the vector s are out of interval [-1,1]")

if (length(params)<3) stop("At least one value in params is missing")
if (length(params)>3) stop("The length of params is 3")

theta<-params[1]
    mu<-params[2]
    a<-params[3]
if (theta<=0)
stop ("Parameter theta must be positive")
if (mu<=0)
stop ("Parameter mu must be positive")
if (a<=0)
stop ("Parameter a must be positive")
    (1+mu/(a*theta)-mu/(a*theta)*exp(theta*(s-1)))^(-a)
}


pgfDpascalpoisson <- function(s,params) {
k<-s[abs(s)>1]


if (length(k)>0)

warning("At least one element of the vector s are out of interval [-1,1]")

if (length(params)<3) stop("At least one value in params is missing")
if (length(params)>3) stop("The length of params is 3")

theta<-params[1]
    mu<-params[2]
    a<-params[3]
if (theta<=0)
stop ("Parameter theta must be positive")
if (mu<=0)
stop ("Parameter mu must be positive")
if (a<=0)
stop ("Parameter a must be positive")
    mu*exp(theta*(s-1))*(1+mu/(a*theta)-mu/(a*theta)*exp(theta*(s-1)))^(-a-1)
}



pgfIpascalpoisson <- function(s,params) {
k<-s[abs(s)>1]


if (length(k)>0)

warning("At least one element of the vector s are out of interval [-1,1]")

if (length(params)<3) stop("At least one value in params is missing")
if (length(params)>3) stop("The length of params is 3")

theta<-params[1]
    mu<-params[2]
    a<-params[3]
if (theta<=0)
stop ("Parameter theta must be positive")
if (mu<=0)
stop ("Parameter mu must be positive")
if (a<=0)
stop ("Parameter a must be positive")
    zval<-1+mu/(a*theta)-s^(-1/a)
    1+log(a*theta*zval/mu)/theta
}

# Logarithmic-Binomial distribution
pgflogarithmicbinomial <- function(s,params) {
k<-s[abs(s)>1]


if (length(k)>0)

warning("At least one element of the vector s are out of interval [-1,1]")

if (length(params)<3) stop("At least one value in params is missing")
if (length(params)>3) stop("The length of params is 3")

    theta<-params[1]
    p<-params[2]
    n<-params[3]

if ((theta>=1)|(theta<=0))
stop ("Parameter theta belongs to the interval (0,1)")

if ((p>=1)|(p<=0))
stop ("Parameter p belongs to the interval (0,1)")
if (n<0)
     stop("Parameter n must be positive")
 if(!(abs(n-round(n))<.Machine$double.eps^0.5))
stop("Parameter n must be positive integer")

    log(1-(1-theta)*(1-p+p*s)^n)/log(theta)
}
pgfDlogarithmicbinomial <- function(s,params) {
    
k<-s[abs(s)>1]


if (length(k)>0)

warning("At least one element of the vector s are out of interval [-1,1]")

if (length(params)<3) stop("At least one value in params is missing")
if (length(params)>3) stop("The length of params is 3")

    theta<-params[1]
    p<-params[2]
    n<-params[3]

if ((theta>=1)|(theta<=0))
stop ("Parameter theta belongs to the interval (0,1)")

if ((p>=1)|(p<=0))
stop ("Parameter p belongs to the interval (0,1)")
if (n<0)
     stop("Parameter n must be positive")
 if(!(abs(n-round(n))<.Machine$double.eps^0.5))
stop("Parameter n must be positive integer")

    -n*p*(1-theta)/log(theta)*(1-p+p*s)^(n-1)*(1-(1-theta)*(1-p+p*s)^n)^(-1)
}


pgfIlogarithmicbinomial <- function(s,params) {
k<-s[abs(s)>1]


if (length(k)>0)

warning("At least one element of the vector s are out of interval [-1,1]")

if (length(params)<3) stop("At least one value in params is missing")
if (length(params)>3) stop("The length of params is 3")

    theta<-params[1]
    p<-params[2]
    n<-params[3]

if ((theta>=1)|(theta<=0))
stop ("Parameter theta belongs to the interval (0,1)")

if ((p>=1)|(p<=0))
stop ("Parameter p belongs to the interval (0,1)")
if (n<0)
     stop("Parameter n must be positive")
 if(!(abs(n-round(n))<.Machine$double.eps^0.5))
stop("Parameter n must be positive integer")
    zval<-(1-theta^s)/(1-theta)
    (zval^(1/n)-1+p)/p
}





# Logarithmic-Poisson distribution
pgflogarithmicpoisson <- function(s,params) {
k<-s[abs(s)>1]


if (length(k)>0)

warning("At least one element of the vector s are out of interval [-1,1]")


if (length(params)<2) stop("At least one value in params is missing")
if (length(params)>2) stop("The length of params is 2")



    theta<-params[1]
    lambda<-params[2]
if ((theta>=1)|(theta<=0))
stop ("Parameter theta belongs to the interval (0,1)")

if (lambda<=0)
stop ("Parameter lambda must be positive")


    log(1-(1-theta)*exp(lambda*(s-1)))/log(theta)
}






pgfDlogarithmicpoisson <- function(s,params) {
k<-s[abs(s)>1]


if (length(k)>0)

warning("At least one element of the vector s are out of interval [-1,1]")


if (length(params)<2) stop("At least one value in params is missing")
if (length(params)>2) stop("The length of params is 2")



    theta<-params[1]
    lambda<-params[2]
if ((theta>=1)|(theta<=0))
stop ("Parameter theta belongs to the interval (0,1)")

if (lambda<=0)
stop ("Parameter lambda must be positive")

    -lambda*(1-theta)/log(theta)*exp(lambda*(s-1))/(1-(1-theta)*exp(lambda*(s-1)))
}





pgfIlogarithmicpoisson <- function(s,params) {
k<-s[abs(s)>1]


if (length(k)>0)

warning("At least one element of the vector s are out of interval [-1,1]")

if (length(params)<2) stop("At least one value in params is missing")
if (length(params)>2) stop("The length of params is 2")




    theta<-params[1]
    lambda<-params[2]
if ((theta>=1)|(theta<=0))
stop ("Parameter theta belongs to the interval (0,1)")

if (lambda<=0)
stop ("Parameter lambda must be positive")

    zval<-(1-theta^s)/(1-theta)
    1+log(zval)/lambda
}
















# Poisson-Lindley distribution


pgfpoissonlindley <- function(s,params) {
k<-s[abs(s)>1]


if (length(k)>0)

warning("At least one element of the vector s are out of interval [-1,1]")

if (length(params)>1) stop("The length of params is 1")


    theta<-params[1]
if (theta<=0)
stop ("Parameter lambda must be positive")

    theta^2*(theta+2-s)/((theta+1)*(theta+1-s)^2)
}

pgfDpoissonlindley <- function(s,params) {
k<-s[abs(s)>1]


if (length(k)>0)

warning("At least one element of the vector s are out of interval [-1,1]")

if (length(params)>1) stop("The length of params is 1")

    theta<-params[1]
if (theta<=0)
stop ("Parameter lambda must be positive")
    (2/(theta+1-s)-1/(theta+2-s))*pgfpoissonlindley(s,params)
}

pgfIpoissonlindley <- function(s,params) {
    xval<-length(s)
    theta<-params[1]
    for (i in 1:length(s)) {
        func<-function(x) pgfpoissonlindley(x,params)-s[i]
        xval[i]<-uniroot(func,lower=0,upper=1)$root
    }
    xval
}

# Hyper-Poisson distribution
pgfhyperpoisson <- function(s,params) {
    require(hypergeo)
k<-s[abs(s)>1]


if (length(k)>0)

warning("At least one element of the vector s are out of interval [-1,1]")

if (length(params)<2) stop("At least one value in params is missing")
if (length(params)>2) stop("The length of params is 2")

    theta<-params[1]
    lambda<-params[2]

if (theta<=0)
stop ("Parameter theta must be positive")
if (lambda<=0)
stop ("Parameter lambda must be positive")



    genhypergeo(1,lambda,theta*s)/genhypergeo(1,lambda,theta)
}
pgfDhyperpoisson <- function(s,params) {
    require(hypergeo)
k<-s[abs(s)>1]


if (length(k)>0)

warning("At least one element of the vector s are out of interval [-1,1]")

if (length(params)<2) stop("At least one value in params is missing")
if (length(params)>2) stop("The length of params is 2")

    theta<-params[1]
    lambda<-params[2]

if (theta<=0)
stop ("Parameter theta must be positive")
if (lambda<=0)
stop ("Parameter lambda must be positive")


    theta*genhypergeo(2,lambda+1,theta*s)/(lambda*genhypergeo(1,lambda,theta))
}

pgfIhyperpoisson <- function(s,params) {
    xval<-length(s)
    for (i in 1:length(s)) {
        func<-function(x) pgfhyperpoisson(x,params)-s[i]
        xval[i]<-uniroot(func,lower=0,upper=1)$root
    }
    xval
}



# Yule distribution
pgfyule <- function(s,params) {
    require(hypergeo)


k<-s[abs(s)>1]


if (length(k)>0)

warning("At least one element of the vector s are out of interval [-1,1]")

if (length(params)>1) stop("The length of params is 1")
    theta<-params[1]
if (theta<=0)
stop ("Parameter theta must be positive")

    theta/(theta+1)*Re(hypergeo(1,1,theta+2,s))
}

pgfDyule <- function(s,params) {
    require(hypergeo)
k<-s[abs(s)>1]


if (length(k)>0)

warning("At least one element of the vector s are out of interval [-1,1]")

if (length(params)>1) stop("The length of params is 1")
    theta<-params[1]
if (theta<=0)
stop ("Parameter theta must be positive")

    theta/((theta+1)*(theta+2))*Re(hypergeo(2,2,theta+3,s))
}

pgfIyule <- function(s,params) {
    xval<-length(s)
    for (i in 1:length(s)) {
        func<-function(x) pgfyule(x,params)-s[i]
        xval[i]<-uniroot(func,lower=0,upper=1)$root
    }
    xval
}

# Waring distribution
pgfwaring <- function(s,params) {
    require(hypergeo)
k<-s[abs(s)>1]


if (length(k)>0)

warning("At least one element of the vector s are out of interval [-1,1]")

if (length(params)<2) stop("At least one value in params is missing")
if (length(params)>2) stop("The length of params is 2")

    cc<-params[1]
    a<-params[2]
if (cc<=0)
stop ("Parameter c must be positive")
if (a<=0)
stop ("Parameter a must be positive")


    (cc-a)/cc*Re(hypergeo(1,a,cc+1,s))
}


pgfDwaring <- function(s,params) {
    require(hypergeo)
k<-s[abs(s)>1]


if (length(k)>0)

warning("At least one element of the vector s are out of interval [-1,1]")

if (length(params)<2) stop("At least one value in params is missing")
if (length(params)>2) stop("The length of params is 2")

    cc<-params[1]
    a<-params[2]
if (cc<=0)
stop ("Parameter c must be positive")
if (a<=0)
stop ("Parameter a must be positive")

    a*(cc-a)/(cc*(cc+1))*Re(hypergeo(2,a+1,cc+2,s))
}


pgfIwaring <- function(s,params) {
    xval<-length(s)
    for (i in 1:length(s)) {
        func<-function(x) pgfwaring(x,params)-s[i]
        xval[i]<-uniroot(func,lower=0,upper=1)$root
    }
    xval
}




# Katti type H1 distribution
pgfkattitypeh1 <- function(s,params) {
    require(hypergeo)
k<-s[abs(s)>1]


if (length(k)>0)

warning("At least one element of the vector s are out of interval [-1,1]")

if (length(params)<3) stop("At least one value in params is missing")
if (length(params)>3) stop("The length of params is 3")


    theta<-params[1]
    a<-params[2]
    b<-params[3]

if (theta<=0)
stop ("Parameter theta must be positive")
if (a<=0)
stop ("Parameter a must be positive")
if (b<=0)
stop ("Parameter b must be positive")

    genhypergeo(a,a+b,theta*(s-1))
}




pgfDkattitypeh1 <- function(s,params) {
    require(hypergeo)
k<-s[abs(s)>1]


if (length(k)>0)

warning("At least one element of the vector s are out of interval [-1,1]")

if (length(params)<3) stop("At least one value in params is missing")
if (length(params)>3) stop("The length of params is 3")


    theta<-params[1]
    a<-params[2]
    b<-params[3]

if (theta<=0)
stop ("Parameter theta must be positive")
if (a<=0)
stop ("Parameter a must be positive")
if (b<=0)
stop ("Parameter b must be positive")

    a*theta/(a+b)*genhypergeo(a+1,a+b+1,theta*(s-1))
}




pgfIkattitypeh1 <- function(s,params) {
    xval<-length(s)
    for (i in 1:length(s)) {
        func<-function(x) pgfkattitypeh1(x,params)-s[i]
        xval[i]<-uniroot(func,lower=0,upper=1)$root
    }
    xval
}

# Katti type H2 distribution

pgfkattitypeh2 <- function(s,params) {
    require(hypergeo)
k<-s[abs(s)>1]


if (length(k)>0)

warning("At least one element of the vector s are out of interval [-1,1]")

if (length(params)<4) stop("At least one value in params is missing")
if (length(params)>4) stop("The length of params is 4")

    theta<-params[1]
    a<-params[2]
    b<-params[3]
    k<-params[4]

if (theta<=0)
stop ("Parameter theta must be positive")
if (a<=0)
stop ("Parameter a must be positive")
if (b<=0)
stop ("Parameter b must be positive")
if (k<=0)
stop ("Parameter k must be positive")

    Re(hypergeo(k,a,a+b,theta*(s-1)))
}





pgfDkattitypeh2 <- function(s,params) {
    require(hypergeo)
k<-s[abs(s)>1]


if (length(k)>0)

warning("At least one element of the vector s are out of interval [-1,1]")

if (length(params)<4) stop("At least one value in params is missing")
if (length(params)>4) stop("The length of params is 4")

    theta<-params[1]
    a<-params[2]
    b<-params[3]
    k<-params[4]

if (theta<=0)
stop ("Parameter theta must be positive")
if (a<=0)
stop ("Parameter a must be positive")
if (b<=0)
stop ("Parameter b must be positive")
if (k<=0)
stop ("Parameter k must be positive")    

a*k*theta/(a+b)*Re(hypergeo(k+1,a+1,a+b+1,theta*(s-1)))
}


pgfIkattitypeh2 <- function(s,params) {
    xval<-length(s)
    for (i in 1:length(s)) {
        func<-function(x) pgfkattitypeh2(x,params)-s[i]
        xval[i]<-uniroot(func,lower=0,upper=1)$root
    }
    xval
}

# Hypergeometric distribution

pgfhypergeometric <- function(s,params) {
    require(hypergeo)

k<-s[abs(s)>1]


if (length(k)>0)

warning("At least one element of the vector s are out of interval [-1,1]")

if (length(params)<3) stop("At least one value in params is missing")
if (length(params)>3) stop("The length of params is 3")


    m<-params[1]
    n<-params[2]
    p<-params[3]

if (m<0)
     stop("Parameter m must be positive")
 if(!(abs(m-round(m))<.Machine$double.eps^0.5))
stop("Parameter m must be positive integer")

if (n<0)
     stop("Parameter n must be positive")
 if(!(abs(n-round(n))<.Machine$double.eps^0.5))
stop("Parameter n must be positive integer")
if ((p>=1)|(p<=0))
stop ("Parameter p belongs to the interval (0,1)")

if (m<n)
stop ("Parameter m is greater or equal then n ")
    Re(hypergeo(-n,-m*p,-m,1-s))
}





pgfDhypergeometric <- function(s,params) {
    require(hypergeo)
k<-s[abs(s)>1]


if (length(k)>0)

warning("At least one element of the vector s are out of interval [-1,1]")

if (length(params)<3) stop("At least one value in params is missing")
if (length(params)>3) stop("The length of params is 3")


    m<-params[1]
    n<-params[2]
    p<-params[3]

if (m<0)
     stop("Parameter m must be positive")
 if(!(abs(m-round(m))<.Machine$double.eps^0.5))
stop("Parameter m must be positive integer")

if (n<0)
     stop("Parameter n must be positive")
 if(!(abs(n-round(n))<.Machine$double.eps^0.5))
stop("Parameter n must be positive integer")
if ((p>=1)|(p<=0))
stop ("Parameter p belongs to the interval (0,1)")

if (m<n)
stop ("Parameter m is greater or equal then n ")

    n*p*Re(hypergeo(1-n,1-m*p,1-m,1-s))
}



pgfIhypergeometric <- function(s,params) {
    xval<-length(s)
    for (i in 1:length(s)) {
        func<-function(x) pgfhypergeometric(x,params)-s[i]
        xval[i]<-uniroot(func,lower=0,upper=1)$root
    }
    xval
}



# Thomas distribution



pgfthomas <- function(s,params) {
k<-s[abs(s)>1]


if (length(k)>0)

warning("At least one element of the vector s are out of interval [-1,1]")

if (length(params)<2) stop("At least one value in params is missing")
if (length(params)>2) stop("The length of params is 2")

    lambda<-params[1]
    theta<-params[2]
if (lambda<=0)
stop ("Parameter lambda must be positive")
if (theta<=0)
stop ("Parameter theta must be positive")

    exp(lambda*(s*exp(theta*(s-1))-1))
}



pgfDthomas <- function(s,params) {
k<-s[abs(s)>1]


if (length(k)>0)

warning("At least one element of the vector s are out of interval [-1,1]")

if (length(params)<2) stop("At least one value in params is missing")
if (length(params)>2) stop("The length of params is 2")

    lambda<-params[1]
    theta<-params[2]
if (lambda<=0)
stop ("Parameter lambda must be positive")
if (theta<=0)
stop ("Parameter theta must be positive")
    lambda*(1+theta*s)*exp(theta*(s-1))*exp(lambda*(s*exp(theta*(s-1))-1))
}



pgfIthomas <- function(s,params) {
    xval<-length(s)
    for (i in 1:length(s)) {
        func<-function(x) pgfthomas(x,params)-s[i]
        xval[i]<-uniroot(func,lower=0,upper=1)$root
    }
    print(xval)
    xval
}


################# Main programs #######################################

# Cumulative distribution function
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

# Probability density function
dCompound <- function(x,parent,compound,compoundDist,params,...) {
    if (!exists(paste("p",parent,sep=""))) {
        return(paste("The parent distribution",parent,"doesn't exist"))
    }
    if (!is.element(compound,compoundDist)) {
        return(paste("The discrete distribution",compound,"doesn't exist"))
    }
    xval <- real(length(x))
    f <- get(paste("d", parent, sep = ""), mode = "function")
    F <- get(paste("p", parent, sep = ""), mode = "function")
    phi <- get(paste("pgf",compound,sep=""), mode = "function")
    phiD <- get(paste("pgfD",compound,sep=""), mode = "function")
    xval <- phiD(1-F(x,...),params)*f(x,...)/(1-phi(0,params))
    return(xval)
}

# Quantile function
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

# Hazard rate function
hCompound <- function(x, parent, compound,compoundDist,params, ...) {
    if (!exists(paste("p",parent,sep=""))) {
        return(paste("The parent distribution",parent,"doesn't exist"))
    }
    if (!is.element(compound,compoundDist)) {
        return(paste("The discrete distribution",compound,"doesn't exist"))
    }
    dCompound(x,parent,compound,compoundDist,params,...)/(1-pCompound(x,parent,compound,compoundDist,params,...))
}

# Random number generation
rCompound <- function(n, parent, compound,compoundDist,params, ...) {
    if (!exists(paste("p",parent,sep=""))) {
        return(paste("The parent distribution",parent,"doesn't exist"))
    }
    if (!is.element(compound,compoundDist)) {
        return(paste("The discrete distribution",compound,"doesn't exist"))
    }
    

if (n<0)
     stop("Parameter n must be positive")
 if(!(abs(n-round(n))<.Machine$double.eps^0.5))
stop("Parameter n must be positive integer")
if ((p>=1)|(p<=0))
stop ("Parameter p belongs to the interval (0,1)")

if (m<n)
stop ("Parameter m is greater or equal then n ")

    zval <- runif(n)
    xval <- qCompound(zval,parent,compound,compoundDist,params,...)
    return(xval)
}

# Derivation of moments of any order
momentCompound <- function(k, parent, compound,compoundDist, params, ...) {
    if (!exists(paste("p",parent,sep=""))) {
        return(paste("The parent distribution",parent,"doesn't exist"))
    }
    if (!is.element(compound,compoundDist)) {
        return(paste("The discrete distribution",compound,"doesn't exist"))
    }
    if (k<0)
     stop("Parameter k must be positive")
 if(!(abs(k-round(k))<.Machine$double.eps^0.5))
stop("Parameter k must be positive integer")

    Finv <- get(paste("q", parent, sep = ""), mode = "function")
    phi <- get(paste("pgf",compound,sep=""), mode = "function")
    phiD <- get(paste("pgfD",compound,sep=""), mode = "function")
    fint <- function(x) phiD(1-x,params)*(Finv(x,...))^k/(1-phi(0,params))
    return(integrate(fint,lower=0,upper=1)$value)
}

# Expectation
meanCompound <- function(parent,compound,compoundDist,params,...) {
    if (!exists(paste("p",parent,sep=""))) {
        return(paste("The parent distribution",parent,"doesn't exist"))
    }
    if (!is.element(compound,compoundDist)) {
        return(paste("The discrete distribution",compound,"doesn't exist"))
    }
    return(momentCompound(1,parent,compound,compoundDist,params,...))
}

# Variance
varCompound <- function(parent,compound,compoundDist,params,...) {
    if (!exists(paste("p",parent,sep=""))) {
        return(paste("The parent distribution",parent,"doesn't exist"))
    }
    if (!is.element(compound,compoundDist)) {
        return(paste("The discrete distribution",compound,"doesn't exist"))
    }
    m1 <- momentCompound(1,parent,compound,compoundDist,params,...)
    m2 <- momentCompound(2,parent,compound,compoundDist,params,...)
    return(m2-m1^2)
}

# Skewness
skewCompound <- function(parent,compound,compoundDist,params,...) {
    if (!exists(paste("p",parent,sep=""))) {
        return(paste("The parent distribution",parent,"doesn't exist"))
    }
    if (!is.element(compound,compoundDist)) {
        return(paste("The discrete distribution",compound,"doesn't exist"))
    }
   m1 <- meanCompound(parent,compound,compoundDist,params,...)
   m3 <- momentCompound(3,parent,compound,compoundDist,params,...)
   sig2 <- varCompound(parent,compound,compoundDist,params,...)
   return((m3-3*m1*sig2-m1^3)/sig2^(3/2))
}

# Kurtosis
kurtCompound <- function(parent,compound,compoundDist,params,...) {
    if (!exists(paste("p",parent,sep=""))) {
        return(paste("The parent distribution",parent,"doesn't exist"))
    }
    if (!is.element(compound,compoundDist)) {
        return(paste("The discrete distribution",compound,"doesn't exist"))
    }
   m1 <- meanCompound(parent,compound,compoundDist,params,...)
   m3 <- momentCompound(3,parent,compound,compoundDist,params,...)
   m4 <- momentCompound(4,parent,compound,compoundDist,params,...)
   sig2 <- varCompound(parent,compound,compoundDist,params,...)
   return((m4-4*m1*m3+6*m1^2*sig2+3*m1^4)/sig2^2)
}

