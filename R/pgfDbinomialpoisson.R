pgfDbinomialpoisson <-
function(s,params) {
    theta<-params[1]
    p<-params[2]
    n<-params[3]
    n*theta*p*exp(theta*(s-1))*(1-p+p*exp(theta*(s-1)))^(n-1)
}

