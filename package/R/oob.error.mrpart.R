oob.error.mrpart <- function(object, data, formula)
{
 oobmat <- .oobMatrix(object, data, formula)

 oobclass <- apply(oobmat, 1, function(x)names(which.max(x)))

 target <- .getTarget(data, formula)

 forest.oob.error <- sum(oobclass != data[names(oobclass), target])/length(oobclass)

 return(forest.oob.error)
}
