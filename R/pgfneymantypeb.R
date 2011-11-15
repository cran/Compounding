pgfneymantypeb <-
function(s,params) {
    require(hypergeo)
    theta<-params[1]
    lambda<-params[2]
    exp(lambda*(genhypergeo(1,2,theta*(s-1))-1))
}

