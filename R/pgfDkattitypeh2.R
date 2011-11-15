pgfDkattitypeh2 <-
function(s,params) {
    require(hypergeo)
    theta<-params[1]
    a<-params[2]
    b<-params[3]
    k<-params[4]
    a*k*theta/(a+b)*Re(hypergeo(k+1,a+1,a+b+1,theta*(s-1)))
}

