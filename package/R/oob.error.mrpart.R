.oob2.error.mrpart <- function(object)
{
  scores <- lapply(object, function(x) x$oob.scores)

  whichClassMax <- function(x)
  {
    names(which.max(x))
  }

  mapToMaxClass <- function(x)
  {
    data.frame(id=row.names(x),
               row.names=NULL,
               class=apply(x, 1, whichClassMax))
  }


  classes <- lapply(scores, mapToMaxClass)

  class.list <- Reduce(rbind, classes)

  class.table <- table(class.list)

  # Actually want to return the oob error measure, not the table.
  
  return(class.table)
}    


.oob1.error.mrpart <- function(forest,DS)
{
  for (i in 1:length(forest))
  {
    osi <- data.frame(id=row.names(forest[[i]]$oob.scores),
                      row.names=NULL,
                      class=apply(forest[[i]]$oob.scores, 1,
                        function(r) names(which.max(r))))
    if (i == 1)  flist <- osi else 
    flist <- rbind(flist, osi)
  }
  tb <- table(flist)
  mat <- matrix(as.vector(tb), nrow=length(levels(flist$id)))
  row.names(mat) <- levels(flist$id)
  colnames(mat) <- levels(flist$class)
  res <- apply(mat, 1, function(x)names(which.max(sort(x))))
  forest.oob.error <- sum(res != DS$data[as.numeric(names(res)), DS$target], na.rm = TRUE)/(length(res)-length(DS$data[as.numeric(names(res)), DS$target][is.na(DS$data[as.numeric(names(res)), DS$target])]))
  return(forest.oob.error)
}
