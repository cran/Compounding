pgfkattitypeh2 <-
function(s,params) {
    require(hypergeo)
    theta<-params[1]
    a<-params[2]
    b<-params[3]
    k<-params[4]
    Re(hypergeo(k,a,a+b,theta*(s-1)))
}

