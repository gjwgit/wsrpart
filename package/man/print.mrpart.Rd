\name{print.mrpart}
\alias{print.mrpart}
\title{

  Print a Multiple Rpart Object
  
}
\description{

  This function prints an 'mrpart' object. It is a method for the
  generic function 'print' of class 'mrpart'.
  
}
\usage{

\method{print}{mrpart}(x, \dots)

}
\arguments{

  \item{x}{fitted model object of class 'mrpart'.  This is assumed to be
          alist of models where ach element in the list is the result of
          some function that produces an object with the same named
          components as that returned by the 'rpart' function.}

  \item{...}{arguments to be passed to or from other methods.}

}
\details{

  This function is a method for the generic function 'print' for class
  '"mrpart"'.  It can be invoked by calling print for an object of the
  appropriate class, or directly by calling 'print.mrpart' regardless of
  the class of the object.
  
}
\author{

  Zhalama, Graham Williams (SIAT)

}
\seealso{

  See Also \code{\link{print.rpart}}, \code{\link{wsrpart}},
  \code{\link{mcwsrpart}}, and \code{\link{clwsrpart}}, .
  
}
\examples{
# A simple example. We receive a different selection of variables
# each time. Here we print two 'mrpart' objects, each consisting
# of just a single decision tree.

print(wsrpart(Species ~ ., iris, nvars=2))
print(wsrpart(Species ~ ., iris, nvars=2))

# A more realistic example using a dataset from the rattle package and
# the Dataset container from the container package.

if(require(rattle) && require(container))
{
  weatherDS <- Dataset$new(data=weather,
                           target="RainTomorrow",
                           risk="RISK_MM",
                           ignore=c("Date", "Location"))
}
if (exists("weatherDS")) with(weatherDS, wsrpart(form, data[vars]))
if (exists("weatherDS")) with(weatherDS, wsrpart(form, data[vars]))

# A slightly more convoluted approach (arguably), not using containers.

if (require(rattle))
  wsrpart(RainTomorrow ~ .,
             weather[which(! names(weather) \%in\%
                             c("Date", "Location", "RISK_MM"))])

if (require(rattle))
  wsrpart(RainTomorrow ~ .,
             weather[which(! names(weather) \%in\%
                             c("Date", "Location", "RISK_MM"))])

}

