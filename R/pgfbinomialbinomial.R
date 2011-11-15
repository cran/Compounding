pgfbinomialbinomial <-
function(s,params) {
    p1<-params[1]
    p2<-params[2]
    m<-params[3]
    n<-params[4]
    (1-p1+p1*(1-p2+p2*s)^n)^m
}

