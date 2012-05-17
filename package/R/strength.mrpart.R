strength <- function(object, data, formula, ...) UseMethod("strength")

strength.mrpart <- function(object, data, formula)
{

  target <- .getTarget(data, formula)

  oobmat <- oobmatrix(object, data, formula)

  pY <- sapply(row.names(oobmat),
               function(x) oobmat[x, data[x, target]]/sum(oobmat[x,]))

  pMaxJ <-  sapply(row.names(oobmat),
                   function(x) max(oobmat[x, colnames(oobmat) !=
                                          data[x, target]])/sum(oobmat[x,]))

  strength <- mean(pY - pMaxJ)

  return(strength)
}
