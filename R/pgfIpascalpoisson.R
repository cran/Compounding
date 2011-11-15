pgfIpascalpoisson <-
function(s,params) {
    theta<-params[1]
    mu<-params[2]
    a<-params[3]
    zval<-1+mu/(a*theta)-s^(-1/a)
    1+log(a*theta*zval/mu)/theta
}

