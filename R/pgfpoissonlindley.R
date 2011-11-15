pgfpoissonlindley <-
function(s,params) {
    theta<-params[1]
    theta^2*(theta+2-s)/((theta+1)*(theta+1-s)^2)
}

