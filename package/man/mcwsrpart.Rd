\name{mcwsrpart}
\alias{mcwsrpart}

\title{
 
 Building the forests of trees over the cores of one server.

}
\description{

Making us of the cores of a server through the use of mcparallel(), we build the forests of trees. 

}
\usage{

mcwsrpart(DS, ntrees, ncores = detectCores(), ...)

}

\arguments{
  \item{DS}{
Dataset environment.
}
  \item{ntrees}{
The number of trees to build on each server.
}
  \item{ncores}{
The number of trees to be used.
}
  \item{\dots}{
Arguments passed on to mcwsrpart() and rpart().
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
%%\examples{
%% ##---- Should be DIRECTLY executable !! ----
%%##-- ==>  Define data, use random,
%%##--	or do  help(data=index)  for the standard data sets.

%%## The function is currently defined as
%%function (DS, ntrees, ncores = detectCores(), ...) 
%%{
  %%  if (ncores < 1) 
   %%     stop("ncores must be a positive integer")
   %% if (ntrees < 1) 
   %%     stop("ntrees must be a positive integer")
   %% require(parallel)
    %%nruns <- ntrees\%/\%ncores
   %% extra <- ntrees\%\%ncores
   %% forest <- list()
 %%   for (i in seq_len(nruns)) {
  %%      jobs <- lapply(1:ncores, function(x) mcparallel(wsrpart(DS, 
  %%          ...)))
  %%      forest <- c(forest, mccollect(jobs, wait = TRUE))
  %%  }
  %%  jobs <- lapply(seq_len(extra), function(x) mcparallel(wsrpart(DS, 
  %%      ...)))
  %%  forest <- c(forest, mccollect(jobs, wait = TRUE))
  %%  class(forest) <- "mrpart"
  %%  return(forest)
 %% }
%%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
