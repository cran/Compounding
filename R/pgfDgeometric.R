pgfDgeometric <-
function(s,params) {
    theta<-params[1]
    theta*(1-theta)/(1-(1-theta)*s)^2
}

