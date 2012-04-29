selectVars <-
function(formula, data, n)
{
  pvar <- varWeights(formula, data)
  draws <- sample(names(pvar), size=n, replace=FALSE, prob=pvar) 
  return(draws)
}
