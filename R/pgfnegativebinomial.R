pgfnegativebinomial <-
function(s,params) {
    theta<-params[1]
    k<-params[2]
    (theta/(1-(1-theta)*s))^k
}

