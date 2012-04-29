library(wsrpart)

set.seed(42)

selectVars(Species ~ ., iris, 2)
selectVars(Species ~ ., iris, 2)
selectVars(Species ~ ., iris, 2)
selectVars(Species ~ ., iris, 2)

if(require(rattle) && require(container))
{
  weatherDS <- Dataset$new(data=weather,
                           target="RainTomorrow",
                           risk="RISK_MM",
                           ignore=c("Date", "Location"))
}
if (exists("weatherDS")) with(weatherDS, selectVars(form, data[vars], 5))
if (exists("weatherDS")) with(weatherDS, selectVars(form, data[vars], 5))
if (exists("weatherDS")) with(weatherDS, selectVars(form, data[vars], 5))
if (exists("weatherDS")) with(weatherDS, selectVars(form, data[vars], 5))

if (require(rattle))
  selectVars(RainTomorrow ~ .,
             weather[which(! names(weather) %in%
                             c("Date", "Location", "RISK_MM"))], 5)

if (require(rattle))
  selectVars(RainTomorrow ~ .,
             weather[which(! names(weather) %in%
                             c("Date", "Location", "RISK_MM"))], 5)
