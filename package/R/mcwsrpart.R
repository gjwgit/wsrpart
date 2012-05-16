mcwsrpart <-
function(formula, data, ntrees=50, ncores=detectCores(), ...)
{
  if (missing(ntrees))
    stop("ntrees= must be provided as an argument")
  
  if (ntrees < 1)
    stop("ntrees= must be a positive integer")

  if (ncores < 1)
    stop("ncores= must be a positive integer")

  require(parallel)
  
  # Start a parallel run. Needs to somehow schedule up to 
  # just ncores runs at one time, then kick in the next one as each 
  # core becomes available, until ntrees trees have been built.
  
  nruns <- ntrees %/% ncores
  extra <- ntrees %% ncores
  
  forest <- list()
  
  for (i in seq_len(nruns))
  {
    jobs <- lapply(1:ncores, function(x) mcparallel(wsrpart(formula, data, ...)))

    # Collect results.
  
    forest <- c(forest, mccollect(jobs, wait=TRUE))
  }
  
  jobs <- lapply(seq_len(extra), function(x) mcparallel(wsrpart(formula, data, ...)))
  forest <- c(forest, mccollect(jobs, wait=TRUE))
  forest <- Reduce(c, forest)
  
  # The result is of class mrpart - multiple rpart - with each 
  # element having at least one element named model.
  
  class(forest) <- "mrpart"
  
  # Return list of results.
  
  return(forest)
}
