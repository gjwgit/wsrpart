\name{predict.mrpart}
\alias{predict.mrpart}
\title{

  Predict method for a multiple rpart object.
  
}
\description{
  
  Returns a vector of predicted responses from an `mrpart' object.

}
\usage{

predict.mrpart(object, newdata, type=c("vector", "prob", "class", "votes"), ...)

}
\arguments{
  \item{object}{
    
    a model of class `mrpart'. This is created by one of the model
    builders from the `wsrpart' package. An `mrpart' model is
    essentially a forest of `rpart' decision trees.
    
  }
  \item{newdata}{
    
    a data frame containing observations for which predictions are
    required. The predictors referred to in the right side of
    `formula(object)' must be present by name in `newdata'.
    
  }
  \item{type}{

    a character string denoting the type of predicted value returned.
    The default is to return the `class' or predicted  value. `prob'
    returns a matrix of class probabilities and `votes' returns a matrix
    of vote counts.
    
  }
}
\details{

  This function is a method for the generic function predict for class
  `mrpart'. It can be invoked by calling `predict' for an object of the
  appropriate class, or directly by calling `predict.mrpart' regardless
  of the class of the object.

  
}
\value{

  The returned object depends on the argument `type':

  \item{class:}{predicted classes (the classes with majority vote).}

  \item{prob:}{matrix of class probabilities (one column for each class and
  one row for each observation).}

  \item{vote:}{matrix of vote counts (one column for each class and one
  row for each new observation); either in raw counts or in fractions (if
  ‘norm.votes=TRUE’).}
  
}
\author{

  Zhalama and Graham Williams

}
