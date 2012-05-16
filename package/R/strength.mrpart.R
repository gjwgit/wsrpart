strength.mrpart <- function(object, data, formula)
{

  vars <- as.character(attr(terms(model.frame(formula, data)),
                            "variables"))[-1L]
  target <- vars[[1]]

  oob <- sapply(object, function(m) m$oob.scores)

  oobscolist <- list(id=rownames(Reduce(rbind, oob)),
                     class=apply(Reduce(rbind, oob), 1,
                       function(x)names(which.max(x))))

  oobscolist$id <- as.integer(oobscolist$id)

  oobvotes <- table(oobscolist)

  oobmat <- matrix(as.vector(oobvotes),
                   nrow=length(levels(as.factor(oobscolist$id))))
  row.names(oobmat) <- levels(as.factor(oobscolist$id))
  colnames(oobmat) <- levels(as.factor(oobscolist$class))

  pY <- sapply(row.names(oobmat),
               function(x) oobmat[x, data[x, target]]/sum(oobmat[x,]))

  pMaxJ <-  sapply(row.names(oobmat),
                   function(x) max(oobmat[x, colnames(oobmat) !=
                                          data[x, target]])/sum(oobmat[x,]))

  strength <- mean(pY - pMaxJ)

  return(strength)
}
