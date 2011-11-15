pgfDneymantypec <-
function(s,params) {
    require(hypergeo)
    theta<-params[1]
    lambda<-params[2]
    theta*lambda/3*genhypergeo(2,4,theta*(s-1))*pgfneymantypec(s,params)
}

