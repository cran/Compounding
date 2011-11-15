pgfthomas <-
function(s,params) {
    lambda<-params[1]
    theta<-params[2]
    exp(lambda*(s*exp(theta*(s-1))-1))
}

