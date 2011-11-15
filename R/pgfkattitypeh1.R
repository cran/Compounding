pgfkattitypeh1 <-
function(s,params) {
    require(hypergeo)
    theta<-params[1]
    a<-params[2]
    b<-params[3]
    genhypergeo(a,a+b,theta*(s-1))
}

