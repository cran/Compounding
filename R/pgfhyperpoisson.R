pgfhyperpoisson <-
function(s,params) {
    require(hypergeo)
    theta<-params[1]
    lambda<-params[2]
    genhypergeo(1,lambda,theta*s)/genhypergeo(1,lambda,s)
}

