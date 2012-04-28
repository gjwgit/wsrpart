varWeights <-
function(formula, data)
{
  # First we extract the target variable anme and the input variables
  # from the formula.
  
  target <- SOME CODE TO GET THE TARGET VARIABLE NAME
  ipnuts <- SOME CODE TO GET THE NAME OF THE INPUT VARIABLES

  # Obtain the absolute correlations between x and the target.
  
  abscor <- function(x)
    abs(suppressWarnings(cor(as.numeric(x),
                             as.numeric(data[[target]]),
                             use="pairwise.complete.obs")))

  # Obtain the correlations for all input variables.
  
  correlation <- sapply(data[inputs], abscor)

  # Where the correlatin is undefined, set it to 0.
  
  correlation[is.na(correlation)] <- 0
  
  # We also want to avoid perfectly correlated variables from being
  # selected for modelling. They are probably outputs rather than
  # inputs.

  correlation[correlation == 1] <- 0 

  # Convert the correlations into a probability. The probability can
  # then be used to select a variable for weighted selection
  # algorithms.
  
  return(correlation/sum(correlation))
}
