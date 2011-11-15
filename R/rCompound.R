rCompound <-
function(n, parent, compound,compoundDist,params, ...) {
    if (!exists(paste("p",parent,sep=""))) {
        return(paste("The parent distribution",parent,"doesn't exist"))
    }
    if (!is.element(compound,compoundDist)) {
        return(paste("The discrete distribution",compound,"doesn't exist"))
    }
    zval <- runif(n)
    xval <- qCompound(zval,parent,compound,compoundDist,params,...)
    return(xval)
}

