clwsrpart <- function(DS, servers, ntrees, ...)
{
  nodes <- servers
  cl <- makeCluster(nodes)
  clusterEvalQ(cl, require(wsrpart))
  clusterExport(cl, as.character(substitute(DS)))
  forest <- clusterCall(cl, mcwsrpart, DS, ntrees, ...)
  return(Reduce(c, forest))
}
