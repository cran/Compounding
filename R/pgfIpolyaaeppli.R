pgfIpolyaaeppli <-
function(s,params) {
    theta<-params[1]
    p<-params[2]
    (theta+log(s))/(theta+p*log(s))
}

