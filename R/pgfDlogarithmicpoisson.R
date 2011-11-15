pgfDlogarithmicpoisson <-
function(s,params) {
    theta<-params[1]
    lambda<-params[2]
    -lambda*(1-theta)/log(theta)*exp(lambda*(s-1))/(1-(1-theta)*exp(lambda*(s-1)))
}

