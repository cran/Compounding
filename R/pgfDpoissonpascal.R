pgfDpoissonpascal <-
function(s,params) {
    theta<-params[1]
    p<-params[2]
    k<-params[3]
    theta*k*p*(1+p-p*s)^(-k-1)*exp(theta*((1+p-p*s)^(-k)-1))
}

