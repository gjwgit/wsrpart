varWeights <-
function(formula, data)
{

  # Extract the variables from the formula.

  vars <- as.character(attr(terms(model.frame(formula, data)), "variables"))[-1L]
  target <- vars[[1]]
  inputs <- vars[-1]
  
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

