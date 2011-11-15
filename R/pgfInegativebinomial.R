pgfInegativebinomial <-
function(s,params) {
    theta<-params[1]
    k<-params[2]
    (1-theta*s^(-1/k))/(1-theta)
}

