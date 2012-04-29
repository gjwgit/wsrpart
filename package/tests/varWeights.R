library(wsrpart)

varWeights(Species ~ ., iris)

if(require(rattle) && require(container))
{
  weatherDS <- Dataset$new(data=weather,
                           target="RainTomorrow",
                           risk="RISK_MM",
                           ignore=c("Date", "Location"))
  with(weatherDS, varWeights(form, data[vars]))
}

varWeights(RainTomorrow ~ .,
           weather[which(! names(weather) %in%
                           c("Date", "Location", "RISK_MM"))])

