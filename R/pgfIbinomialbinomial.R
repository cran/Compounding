pgfIbinomialbinomial <-
function(s,params) {
    p1<-params[1]
    p2<-params[2]
    m<-params[3]
    n<-params[4]
    zval<-(s^(1/m)-1+p1)/p1
    (zval^(1/n)-1+p2)/p2
}

