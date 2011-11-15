pgfDhypergeometric <-
function(s,params) {
    require(hypergeo)
    m<-params[1]
    n<-params[2]
    p<-params[3]
    n*p*Re(hypergeo(1-n,1-m*p,1-m,1-s))
}

