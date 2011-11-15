pgfpascalpoisson <-
function(s,params) {
    theta<-params[1]
    mu<-params[2]
    a<-params[3]
    (1+mu/(a*theta)-mu/(a*theta)*exp(theta*(s-1)))^(-a)
}

