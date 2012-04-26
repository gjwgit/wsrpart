wsrpart <- function(DS, nvars=trunc(log(DS$ninputs + 1, 2)), ...)
{
  require(rpart)
  
  # Build the new training set each time we call the function as in
  # bagging and random forests. We sample, with replacement, from the
  # dataset container's training set.
  
  tr <- sample(DS$train, length(DS$train), replace=TRUE)
  oob <- setdiff(DS$train, tr)
  
  vr <- selectVars(DS, nvars)
  
  # Build the decision tree model.
  
  model <- rpart(formula=DS$form, data=DS$data[tr, c(vr, DS$target)], ...)

  # We should not really have missing target values, but just in
  # case...
  
  complete <- oob[which(! is.na(DS$data[oob, DS$target]))]

  # This assumes binary classification.
  
  oob.error <- sum((predict(model, DS$data[complete,], type="class") !=
                   DS$data[complete, DS$target])) / length(complete)

  oob.scores <- predict(model, DS$data[oob,])

  result <- list(model=model,
                 vars.used=vr,
                 oob.scores=oob.scores,
                 oob.error=oob.error)

  forest <- list(result)
  class(forest) <- "mrpart"
  
  return(forest)
}
