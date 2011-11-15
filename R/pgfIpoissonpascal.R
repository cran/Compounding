pgfIpoissonpascal <-
function(s,params) {
    theta<-params[1]
    p<-params[2]
    k<-params[3]
    (1+p-(1+log(s)/theta)^(-1/k))/p
}

