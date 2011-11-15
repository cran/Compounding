pgfDneymantypeb <-
function(s,params) {
    require(hypergeo)
    theta<-params[1]
    lambda<-params[2]
    theta*lambda/2*genhypergeo(2,3,theta*(s-1))*pgfneymantypeb(s,params)
}

