pgflogarithmic <-
function(s,params) {
    theta<-params[1]
    log(1-(1-theta)*s)/log(theta)
}

