pgfDbinomial <-
function(s,params) {
    theta<-params[1]
    n<-params[2]
    n*theta*(1-theta+theta*s)^(n-1)
}

