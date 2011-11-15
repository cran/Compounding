pgfbinomial <-
function(s,params) {
    theta<-params[1]
    n<-params[2]
    (1-theta+theta*s)^n
}

