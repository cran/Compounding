pgfwaring <-
function(s,params) {
    require(hypergeo)
    cc<-params[1]
    a<-params[2]
    (cc-a)/cc*Re(hypergeo(1,a,cc+1,s))
}

