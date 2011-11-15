pgflogarithmicbinomial <-
function(s,params) {
    theta<-params[1]
    p<-params[2]
    n<-params[3]
    log(1-(1-theta)*(1-p+p*s)^n)/log(theta)
}

