pgfpoisson <-
function(s,params) {
    theta<-params[1]
    exp(theta*(s-1))
}

