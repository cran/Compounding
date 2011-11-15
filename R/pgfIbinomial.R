pgfIbinomial <-
function(s,params) {
    theta<-params[1]
    n<-params[2]
    (s^(1/n)-1+theta)/theta
}

