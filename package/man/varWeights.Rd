\name{varWeights}
\alias{varWeights}
\title{

  Calculate variable weights based on correlation with target.
  
}
\description{

  Based on the target and input variables identified in the formula,
  generate input variable weights as a probability based on the
  correlation of the input variable to the target variable. The supplied
  data is used to determine the correlations.
  
}
\usage{

varWeights(formula, data)

}

\arguments{

  \item{formula}{a formula identifying the target variable (the
    response), followed by "~", and then the input variables ("." or a
    list of names separated by "+"). Interaction terms are not
    supported.}

  \item{data}{a data frame in which to interpret the variables named in
  the formula and from which the model is built.}

}

\details{

  The probabilities calculated for the input variables are based on
  their correlation to the target variable. It is simply the correlation
  divided by the total sum of all corrlations.

  The probabilities are used to randomly select a subset of variables
  from amongst the available input variables in some relationship to the
  probability of being selected as provided by this function.
  
}
\value{

  A numeric named vector of the probabilities. The names are the
  variable names.

}
\author{

  Zhalama, Graham Williams (SIAT)
}
\seealso{

  See Also \code{\link{selectVars}}.
  
}
\examples{

# A simple example. The weights add up to 1, and can be thought
# of as probabilities.

varWeights(Species ~ ., iris)
sum(varWeights(Species ~ ., iris))

# A more realistic example using a dataset from the rattle package and
# the Dataset container from the container package. The advantage of
# using the Dataset container is that it handles the listing of the
# variables (vars) to be considered as inputs. An alternative is
# also provided below, which shows a littel more complexity for the
# same result.

if(require(rattle) && require(container))
{
  weatherDS <- Dataset$new(data=weather,
                           target="RainTomorrow",
                           risk="RISK_MM",
                           ignore=c("Date", "Location"))
  with(weatherDS, varWeights(form, data[vars]))
}

# A slightly more convoluted approach (arguably), not using containers.

if(require(rattle))
  varWeights(RainTomorrow ~ .,
             weather[which(! names(weather) \%in\%
                             c("Date", "Location", "RISK_MM"))])

}
