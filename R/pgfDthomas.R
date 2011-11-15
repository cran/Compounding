pgfDthomas <-
function(s,params) {
    lambda<-params[1]
    theta<-params[2]
    lambda*(1+theta*s)*exp(theta*(s-1))*exp(lambda*(s*exp(theta*(s-1))-1))
}

