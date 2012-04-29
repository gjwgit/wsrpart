predict.mrpart <-
function(object, newdata, type=c("class", "prob", "votes"), ...)
{
  if (!inherits(object, "mrpart")) 
    stop("Not a legitimate multile rpart object")

  scores <- sapply(object, function(m) predict(m$model, newdata, type="class"))

  scorelist<-data.frame(id=rep(row.names(scores), times=ncol(scores)),
                        row.names=NULL, class=as.vector(scores))
  
  votes <- table(scorelist)

  # The variable votes is a table which can not be operated on easily,
  # so we need to convert votes into a matrix---scoremat.

  scoremat <- matrix(as.vector(votes), nrow=nrow(scores))
  row.names(scoremat) <- row.names(scores)
  colnames(scoremat) <- levels(scorelist$class)

  # Build the class list. Ensure the result is a vector and the values
  # are a factor. This needs to mimic what rpart does. But how is that
  # information lost from the predict above to here? Is the use of
  # "names" below that strips the factor?
  
  classlab <- as.vector(apply(scoremat, 1, function(x) names(which.max(x))))
  names(classlab) <- row.names(newdata)
  classlab <- as.factor(classlab)

  # Get the probabilities.

  prob <- t(apply(scoremat, 1, function(x)x/sum(x)))

  # Return the result.
  
  if (type == "class")
    return(classlab)
  else if (type == "prob")
    return(prob)
  else if (type == "votes")
    return(votes)
  else
    return(list(class = classlab,
                prob = prob,
                votes = votes))
}
