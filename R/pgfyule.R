pgfyule <-
function(s,params) {
    require(hypergeo)
    theta<-params[1]
    theta/(theta+1)*Re(hypergeo(1,1,theta+2,s))
}

