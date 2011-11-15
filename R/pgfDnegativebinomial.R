pgfDnegativebinomial <-
function(s,params) {
    theta<-params[1]
    k<-params[2]
    k*(1-theta)*theta^k/(1-(1-theta)*s)^(k+1)
}

