pgfpolyaaeppli <-
function(s,params) {
    theta<-params[1]
    p<-params[2]
    exp(theta/p*((1-p)/(1-p*s)-1))
}

