mwsrpart <- function(DS, ntrees, ...)
{
  forest <- sapply(1:ntrees, function(x) wsrpart(DS, ...))
  class(forest) <- "mrpart"
  return(forest)
}
