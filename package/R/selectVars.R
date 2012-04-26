selectVars <- function(DS, n)
{
  pvar <- varWeights(DS)
  draws = sample(names(pvar), size=n, replace=FALSE, prob=pvar) 
  return(draws)
}
