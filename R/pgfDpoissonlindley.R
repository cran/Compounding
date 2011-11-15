pgfDpoissonlindley <-
function(s,params) {
    theta<-params[1]
    (2/(theta+1-s)-1/(theta+2-s))*pgfpoissonlindley(s,params)
}

