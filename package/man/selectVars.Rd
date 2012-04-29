\name{selectVars}
\alias{selectVars}
\title{

  Weighted selection of \code{n} inputs from \code{data} based correlation with
  target.
  
}
\description{

  Based on weights calculated according to an input variables'
  correlation to the target variable, select \code{n} input
  variables. The input and target variables are determined from the
  \code{formula}. The correlations are based on the \code{data}.
  
}
\usage{

selectVars(formula, data, n)

}
\arguments{

  \item{formula}{a formula identifying the target variable (the
    response), followed by "~", and then the input variables ("." or a
    list of names separated by "+"). Interaction terms are not
    supported.}

  \item{data}{a data frame in which to interpret the variables named in
    the formula and from which the model is built.}
  
  \item{n}{the number of variables to select.}

}
\details{

  MORE DETAILS?
  
}
\value{

  A character verctor of length \code{n} naming selected variables.
}
\author{

  Zhalama, Graham Williams (SIAT)

}
\seealso{

  See Also \code{\link{varWeights}}.
  
}
\examples{
# A simple example. We receive a different selection each time.

selectVars(Species ~ ., iris, 2)
selectVars(Species ~ ., iris, 2)
selectVars(Species ~ ., iris, 2)
selectVars(Species ~ ., iris, 2)

# A more realistic example using a dataset from the rattle package and
# the Dataset container from the container package. The advantage of
# using the Dataset container is that it handles the listing of the
# variables (vars) to be considered as inputs, and the resulting
# repeated calls to selectVars is a lot clearer. An alternative is
# also provided below, which shows a little more complexity for the
# same process.

if(require(rattle) && require(container))
{
  weatherDS <- Dataset$new(data=weather,
                           target="RainTomorrow",
                           risk="RISK_MM",
                           ignore=c("Date", "Location"))}
if (exists("weatherDS")) with(weatherDS, selectVars(form, data[vars], 5))
if (exists("weatherDS")) with(weatherDS, selectVars(form, data[vars], 5))
if (exists("weatherDS")) with(weatherDS, selectVars(form, data[vars], 5))
if (exists("weatherDS")) with(weatherDS, selectVars(form, data[vars], 5))

# A slightly more convoluted approach (arguably), not using containers.

if (require(rattle))
  selectVars(RainTomorrow ~ .,
             weather[which(! names(weather) \%in\%
                             c("Date", "Location", "RISK_MM"))], 5)

if (require(rattle))
  selectVars(RainTomorrow ~ .,
             weather[which(! names(weather) \%in\%
                             c("Date", "Location", "RISK_MM"))], 5)
}
