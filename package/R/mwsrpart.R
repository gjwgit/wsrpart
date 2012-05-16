mwsrpart <- function(formula, data, ntrees=50, ...)
{
  forest <- sapply(1:ntrees, function(x) wsrpart(formula, data, ...))
  class(forest) <- "mrpart"
  return(forest)
}
