pgfDyule <-
function(s,params) {
    require(hypergeo)
    theta<-params[1]
    theta/((theta+1)*(theta+2))*Re(hypergeo(2,2,theta+3,s))
}

