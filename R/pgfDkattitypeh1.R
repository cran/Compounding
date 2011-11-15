pgfDkattitypeh1 <-
function(s,params) {
    require(hypergeo)
    theta<-params[1]
    a<-params[2]
    b<-params[3]
    a*theta/(a+b)*genhypergeo(a+1,a+b+1,theta*(s-1))
}

