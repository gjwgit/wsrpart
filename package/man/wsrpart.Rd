\name{wsrpart}
\alias{wsrpart}
\alias{mwsrpart}
\alias{mcwsrpart}
\alias{clwsrpart}
\title{

  Build a forest of weighted subspaced decision trees.

}
\description{
  
  Build \link{rpart} decision trees to construct a forest. The formula
  and a random subset of the rows and columns of data are passed on to
  \link{rpart} to build the decision trees.

}
\usage{

wsrpart(formula, data, nvars, ...)

mwsrpart(formula, data, ntrees, ...)

mcwsrpart(formula, data, ntrees, ncores=detectCores(), ...)

clwsrpart(formula, data, ntrees, servers, ...)

}
\arguments{
  
  \item{formula}{a formula, with a response but no interaction terms.}

  \item{data}{a data frame in which to interpret the variables named in
          the formula.}
  
  \item{nvars}{number of variables to choose, with Breiman's default for
    random forests being the integer less than or equal to
    $log_2(ninputs) + 1$.}

  \item{ntrees}{number of trees to build on each server.}
  
  \item{ncores}{number of cores on the current processor to use.}

  \item{servers}{vector of server names or IP addresses.}

  \item{\dots}{passed on to rpart.}
    
}
\details{

  The base function is 'wsrpart'.  It is repsonsible for calling
  \link{rpart} to build the decision trees. The observations contained
  within the dataset are randomly sampled with replacement. The input
  variables used to build the tree are also a random subset of the
  available input variables. The selection of variables is weighted
  according to their correlation to the target variable.

  'mwsrpart' will build a forest of ntrees trees, each built by calling
  'wsrpart'. The trees are built sequentially (i.e., one after the other
  as a single process.

  'mcwsrpart' builds a forest of trees over the multiple cores of one
  server. It makes use of the cores of a server through the use of
  \link{mcparallel} from \link{parallel}.

  'clwsrpart' builds multiple rpart decision trees over a cluster of
  servers. It uses sockets through the use of makeCluster() to build the
  rpart decision trees in parallel, using multile cores if available on
  each server.

}
\value{
  
  A list:

  \item{model }{An rpart object.}
  
  \item{vars.used }{Variables chosen to be used.}

  \item{oob.scores }{The actual scores of the out-of-bag
    observations. This can be used later in the context of a forest to
    provide an out-of-bag error estimate for the forest of trees.}

  \item{oob.error }{The error rate on the out-of-bag data set.}

}
\author{
  
  Zhalama and Graham Williams (SIAT)
  
}
\seealso{
  
  \code{\link{selectVars}} and \code{\link{varWeights}}.

}
\examples{

# A simple example. Note that because we are randomly sampling both
# the observations and input variables, we expect each run to generate
# a different model.

m1 <- wsrpart(Species ~ ., iris, nvars=2)
class(m1)
m1
m2 <- wsrpart(Species ~ ., iris, nvars=2)
m2

# Another example using a dataset from the rattle package and
# the Dataset container from the container package. We first
# construct a container for the dataset.

if(require(rattle) && require(container))
{
  weatherDS <- Dataset$new(data=weather,
                           target="RainTomorrow",
                           risk="RISK_MM",
                           ignore=c("Date", "Location"))
  weatherDS
}

# Now build a couple of models.

if (exists("weatherDS"))
  with(weatherDS, wsrpart(form, data[train, vars]))

if (exists("weatherDS"))
  with(weatherDS, wsrpart(form, data[train, vars]))

# We now have exapmles of the different optoins for building multiple
# trees. First a sequential build of 5 trees on a single core.

if (exists("weatherDS"))
  system.time(m1 <- with(weatherDS, mwsrpart(form, data[train, vars], 50)))

# Now use multiple cores on the single processor. This takes longer!!!
# Probably the overhead of spawning lots of smaller processes. The
# multicore version is useful for much larger model builds.

if (exists("weatherDS"))
  system.time(mc1 <- with(weatherDS, mcwsrpart(form, data[train, vars], 50)))

}
\keyword{ ~models }
\keyword{ ~classif }
