\name{wsrpart}
\alias{wsrpart}
\title{

  Build a single weighted subspaced decision tree.

}\description{
  
  Build an \link{rpart} decision tree from the data and formula
  contained in the dataset object (DS). The observations contained
  within the dataset will be randomly subsetted (using a default sample
  size of 70\%) to build the tree. Also, the variables used to build the
  tree are a random subset of the available variables. The selection of
  variables is weighted according to their correlation to the target.

}\usage{

wsrpart(DS, nvars = trunc(log(DS$ninputs + 1, 2)), ...)

}
\arguments{
  \item{DS}{
    
    A container (i.e., an environment or list) containing at least the
    named objects \bold{data}, \bold{train}, \bold{inputs},
    \bold{ninputs}, \bold{form}, \bold{target}.
    
  }\item{nvars}{

    Number of variables to choose. The default is Breiman's default for
    random forests, being the integer less than or equal to
    $log_2(ninputs) + 1$.
    
  }\item{sample.size}{
    
    Training subset as a proportion.
    
  }\item{\dots}{
    
    Passed on to rpart.
    
  }
}
\details{
  
  The dataset container is often an \link{environment} in which we store
  a dataset together with meta data associated with the dataset. For the
  purposes of wsrpart the dataset itself is of course required. This
  function uses the following

  \bold{DS$data }{The actual dataset as a data frame.}

  \bold{DS\$train }{A vector of integer indicies that represent a
    training dataset from which the model is built.}

  \bold{DS\$inputs }{A character vector (though could be numeric) that is
    used to identify the input variables for the modelling.}

  \bold{DS\$ninputs }{An integer recording the number of input
    variables.}
  
  \bold{DS\$form }{}.

  \bold{DS\$target}{}.

  The function \link{setupDS} from \link{rattle} provides support for
  incorporating meta data into the container.

}
\value{
  
  A list:

  \item{model }{An rpart object}
  
  \item{vars.used }{Variables chosen to be used}

  \item{oob.error }{The error rate on the out-of-bag data set}

  \item{oob.scores }{The actual scores of the out-of-bag
    observations. This can be later used in the context of a forest to
    provide an out-of-bag error estimate for the forest of trees.}

}
\author{
  
  Zhalama and Graham Williams
  
}
\seealso{
  
  \code{\link{help}}, \code{\link{selectVars}},
  \code{\link{varWeights}}, \code{\link{setupDS}}

}
\examples{

require(rattle)
if (compareVersion(packageDescription("rattle")$Version, "2.6.18") < 0)
  stop("Requires rattle version at least 2.6.18.")
dim(weatherAUS)
weatherDS <- new.env()
evalq({
  data <- weatherAUS
  target <- "RainTomorrow"
  ignore <- c("Date", "Location", "RISK_MM")
}, weatherDS)
setupDataset(weatherDS)
ls(weatherDS)

m1 <- wsrpart(weatherDS)
class(m1)
m1
}
\keyword{ ~models }
\keyword{ ~classif }
