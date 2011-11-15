pgfIlogarithmicpoisson <-
function(s,params) {
    theta<-params[1]
    lambda<-params[2]
    zval<-(1-theta^s)/(1-theta)
    1+log(zval)/lambda
}

