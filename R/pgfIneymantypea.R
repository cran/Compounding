pgfIneymantypea <-
function(s,params) {
    theta<-params[1]
    lambda<-params[2]
    1+1/theta*log(1+log(s)/lambda)
}

