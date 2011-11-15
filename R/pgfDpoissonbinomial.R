pgfDpoissonbinomial <-
function(s,params) {
    theta<-params[1]
    p<-params[2]
    n<-params[3]
    n*theta*p*(1-p+p*s)^(n-1)*exp(theta*((1-p+p*s)^n-1))
}

