pgfIbinomialpoisson <-
function(s,params) {
    theta<-params[1]
    p<-params[2]
    n<-params[3]
    zval<-(s^(1/n)-1+p)/p
    1+log(zval)/theta
}

