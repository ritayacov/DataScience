library(readr)
Time_Series_Analysis <- read_csv("DataScience/stats/Time Series Analysis.r")
View(Time_Series_Analysis)

library(readr)
corona_positives <- read_csv("DataScience/data/corona_positives.csv")
View(corona_positives)

library(readr)
corona_positives <- read_csv("DataScience/data/corona_positives.csv", 
                             col_types = cols(test_date = col_date(format = "%Y-%m-%d")))
View(corona_positives)

dts<- ts(corona_positives$positives,start=min(corona_positives$test_date),end=max(corona_positives$test_date),2)
stationary_dts<-decompose(dts)

plot(stationary_dts)

acf(dts)

pacf(dts)

dts_arima <- arima(dts, order=c(1,0,2))
dts_arima

BIC(dts_arima)

library(forecast)
dts_fit <- forecast(dts_arima)
dts_fit

plot(dts_fit)

dts_autoarima <- auto.arima(dts)
dts_autoarima
