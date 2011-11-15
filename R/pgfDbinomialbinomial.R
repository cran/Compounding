pgfDbinomialbinomial <-
function(s,params) {
    p1<-params[1]
    p2<-params[2]
    m<-params[3]
    n<-params[4]
    m*n*p1*p2*(1-p2+p2*s)^(n-1)*(1-p1+p1*(1-p2+p2*s)^n)^(m-1)
}

