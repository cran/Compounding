pgfDpoisson <-
function(s,params) {
    theta<-params[1]
    theta*exp(theta*(s-1))
}

