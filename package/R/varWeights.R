varWeights <- function(DS)
{
  abscor <- function(x)
    abs(suppressWarnings(cor(as.numeric(x),
                             as.numeric(DS$data[[DS$target]]),
                             use="pairwise.complete.obs")))

  correlation <- sapply(DS$data[DS$inputs], abscor)

  correlation[is.na(correlation)] <- 0
  
  # Avoid perfectly correlated variables.

  correlation[correlation == 1] <- 0 

  # Use the correlation proportions as the probability of the variable
  # being randomly selected.
  
  return(correlation/sum(correlation))
}
