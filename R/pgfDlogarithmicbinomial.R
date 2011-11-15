pgfDlogarithmicbinomial <-
function(s,params) {
    theta<-params[1]
    p<-params[2]
    n<-params[3]
    -n*p*(1-theta)/log(theta)*(1-p+p*s)^(n-1)*(1-(1-theta)*(1-p+p*s)^n)^(-1)
}

