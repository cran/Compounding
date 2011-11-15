pgfDhyperpoisson <-
function(s,params) {
    require(hypergeo)
    theta<-params[1]
    lambda<-params[2]
    theta*genhypergeo(2,lambda+1,theta*s)/(lambda*genhypergeo(1,lambda,theta))
}

