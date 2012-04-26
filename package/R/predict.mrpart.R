predict.mrpart <-
  function(object, newdata, type="class")
{
  scores <- sapply(object, function(m) predict(m$model,newdata,type="class"))

  scorelist<-data.frame(id=rep(row.names(scores),times=ncol(scores)),
                        row.names=NULL,class=as.vector(scores))
  votes <- table(scorelist)

  # votes is a table which can not be operated,so we need to convert
  # votes into a matrix-----scoremat

  scoremat <- matrix(as.vector(votes), nrow=nrow(scores))
  row.names(scoremat) <- row.names(scores)
  colnames(scoremat) <- levels(scorelist$class)
  classlab <- apply(scoremat, 1, function(x)names(which.max(x)))

  #get prob

  prob <- t(apply(scoremat, 1, function(x)x/sum(x)))

  #return result
  
  allResult <- list(class = as.data.frame(classlab), prob = prob,
                    votes = votes)
  if (type == "class") return(as.data.frame(classlab))
  else if (type == "prob") return(prob)
  else if (type == "votes") return(votes)
#  else return(allResult)
}
