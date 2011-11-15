pgfIpoisson <-
function(s,params) {
    theta<-params[1]
    1+log(s)/theta
}

