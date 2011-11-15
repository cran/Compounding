pgfDwaring <-
function(s,params) {
    require(hypergeo)
    cc<-params[1]
    a<-params[2]
    a*(cc-a)/(cc*(cc+1))*Re(hypergeo(2,a+1,cc+2,s))
}

