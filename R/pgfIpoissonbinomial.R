pgfIpoissonbinomial <-
function(s,params) {
    theta<-params[1]
    p<-params[2]
    n<-params[3]
    zval<-1+log(s)/theta
    (zval^(1/n)-1+p)/p
}

