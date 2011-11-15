pgflogarithmicpoisson <-
function(s,params) {
    theta<-params[1]
    lambda<-params[2]
    log(1-(1-theta)*exp(lambda*(s-1)))/log(theta)
}

