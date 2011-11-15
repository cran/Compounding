pgfDneymantypea <-
function(s,params) {
    theta<-params[1]
    lambda<-params[2]
    lambda*theta*exp(theta*(s-1))*exp(lambda*(exp(theta*(s-1))-1))
}

