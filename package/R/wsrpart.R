wsrpart <-
function(formula, data, nvars, ...)
{
  require(rpart)

  # Extract the variables from the formula.

  vars <- as.character(attr(terms(model.frame(formula, data)), "variables"))[-1L]
  target <- vars[[1]]
  inputs <- vars[-1]

  # How many vars to select.

  if (missing(nvars)) nvars <- trunc(log(length(inputs) + 1, 2))
  
  # Build the new training set each time we call the function as in
  # bagging and random forests. We sample, with replacement, from the
  # dataset container's training set.
  
   tr <- sample(row.names(data), length(row.names(data)), replace=TRUE)
   oob <- setdiff(row.names(data), tr)

  # Identify the variables from which to build the model.
  
  vr <- selectVars(formula, data, nvars)
  
  # Build the decision tree model.
  
   model <- rpart(formula, data=data[tr, c(vr, target)], ...)

  # We should not really have missing target values, but just in
  # case...
  
  complete <- oob[which(! is.na(data[oob, target]))]

  # This assumes binary classification.
  
  oob.error <- sum((predict(model, data[complete,], type="class") !=
                   data[complete, target])) / length(complete)

  oob.scores <- predict(model, data[complete,])

  # TODO Also return formula and data - unless we are asked not to
  # include data.
  
  result <- list(model=model,
                 vars.used=vr,
                 oob.scores=oob.scores,
                 oob.error=oob.error)

  forest <- list(result)
  class(forest) <- "mrpart"
  
  return(forest)
}
