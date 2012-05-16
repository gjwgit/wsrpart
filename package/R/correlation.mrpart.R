correlation.mrpart <- function(object, data, formula)
{
  vars <- as.character(attr(terms(model.frame(formula, data)),
                            "variables"))[-1L]
  target <- vars[[1]]

  oob <- sapply(object,function(m) m$oob.scores)

  oobscolist <- list(id = rownames(Reduce(rbind, oob)),
                     class = apply(Reduce(rbind, oob), 1,
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
  
  # Variance of mr
  
  varMr <- mean((pY - pMaxJ)^2) - strength^2
  
  # sd()
  
  p1 <- sapply(object, function(x) sum(data[row.names(x$oob.scores), target] ==
                                       apply(x$oob.scores, 1,
                                             function(m)
                                             names(which.max(m))))/nrow(x$oob.scores))

  f1 <- function(t) names(which.max(oobmat[t, colnames(oobmat)!= data[t, target]]))

  p2 <- sapply(object,
               function(x)
               sum(sapply(row.names(x$oob.scores),
                          function(t) names(which.max(oobmat[t, colnames(oobmat) !=
                                                             data[t, target]]))) ==
                   apply(x$oob.scores, 1, function(m) names(which.max(m)))) /
               nrow(x$oob.scores))
  
  sd <- (p1+p2+(p1-p2)^2)^0.5
  
  cor <- varMr/(mean(sd))^2

  return(cor)
}
