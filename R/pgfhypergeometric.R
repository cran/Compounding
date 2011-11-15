pgfhypergeometric <-
function(s,params) {
    require(hypergeo)
    m<-params[1]
    n<-params[2]
    p<-params[3]
    Re(hypergeo(-n,-m*p,-m,1-s))
}

