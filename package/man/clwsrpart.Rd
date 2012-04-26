\name{clwsrpart}
\alias{clwsrpart}
\title{
  
  Build multiple rpart decision trees over a cluster of servers.
  
}
\description{
  
  Creating sockets through the use of makeCluster(), we build rpart
  decisoin trees in parallel, using multile cores if available on each
  server.
  
}
\usage{

clwsrpart(DS, servers, ntrees, ...)

}
\arguments{
  \item{DS}{
    
    Dataset environment.
    
}
\item{servers}{
  
  Vector of server names or IP addresses.
  
}
\item{ntrees}{
  
  The number of trees to build on each server.
  
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
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
