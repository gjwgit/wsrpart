\name{predict.mrpart}
\alias{predict.mrpart}
\title{

  Predict method for a multiple rpart object.
  
}
\description{
  
  Returns a vector of predicted responses from an `mrpart' object.

}
\usage{

\method{predict}{mrpart}(object, newdata, type=c("class", "prob", "votes"), \dots)

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
  \item{\dots}{

    arguments passed on to predict.rpart
    
  }
}
\details{

  This function is a method for the generic function predict for class
  'mrpart'. It can be invoked by calling `predict' for an object of the
  appropriate class, or directly by calling `predict.mrpart' regardless
  of the class of the object.

  
}
\value{

  The returned object depends on the argument 'type':

  \item{class:}{predicted classes (the classes with majority vote).}

  \item{prob:}{matrix of class probabilities (one column for each class and
  one row for each observation).}

  \item{vote:}{matrix of vote counts (one column for each class and one
  row for each new observation); either in raw counts or in fractions (if
  'norm.votes=TRUE').}
  
}
\author{

  Zhalama and Graham Williams (SIAT)

}
\examples{
# A simple example. We receive a different model each time and so
# different predictions.

table(predict(wsrpart(Species ~ ., iris, nvars=2), iris, type="class"))
table(predict(wsrpart(Species ~ ., iris, nvars=2), iris, type="class"))

# Compare with a straight rpart.

table(predict(rpart(Species ~ ., iris), iris, type="class"))

# Another example using a dataset from the rattle package and
# the Dataset container from the container package. We first
# construct a container for the dataset.

if(require(rattle) && require(container))
{
  weatherDS <- Dataset$new(data=weather,
                           target="RainTomorrow",
                           risk="RISK_MM",
                           ignore=c("Date", "Location"))
}

# Now build a model and use it to predict.

if (exists("weatherDS"))
  with(weatherDS, head(predict(wsrpart(form, data[train, vars]),
                               data[test, vars], type="class")))

# A table gives a better indication of the scores.

if (exists("weatherDS"))
  with(weatherDS, table(predict(wsrpart(form, data[train, vars]),
                                data[test, vars], type="class")))

# Build another model and use it to predict. We expect different
# results.

if (exists("weatherDS"))
  with(weatherDS, table(predict(wsrpart(form, data[train, vars]),
                                data[test, vars], type="class")))

# For comparison build an rpart model.

if (exists("weatherDS"))
  with(weatherDS, table(predict(rpart(form, data[train, vars]),
                                data[test, vars], type="class")))

if (exists("weatherDS"))
  with(weatherDS, head(predict(rpart(form, data[train, vars]),
                               data[test, vars], type="class")))

}
