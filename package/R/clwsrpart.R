clwsrpart <-
function(formula, data, ntrees=50, servers, ...)
{
  nodes <- servers
  cl <- makeCluster(nodes)
  clusterEvalQ(cl, require(wsrpart))
  clusterExport(cl, as.character(substitute(data)))
  forest <- clusterCall(cl, mcwsrpart, formula, data, ntrees, ...)
  result <- Reduce(c, forest)
  class(result) <- "mrpart"
  return(result)
}
