install.packages("tseries")
install.packages("forecast")
library(MASS)
library(tseries)
library(forecast)

Pepperfry <- read.csv("Pepperfry.csv")
colnames(Pepperfry)
exp1 <- ts(Pepperfry[,1],frequency = 28)
plot.ts(exp1, xlab='Month', ylab='Closing Price', main='Time Series Plot', col='navy')


Instock <- log(Pepperfry$Week.Sales[])
Instock
acf(lnstock , lag.max = 20)
pacf(lnstock , lag.max = 20)
difflnstock <-diff(lnstock ,1)
difflnstock
adf.test(lnstock)
adf.test(difflnstock)

salesarima <- ts(lnstock ,start = c(2019,20),  frequency = 12)
fitlnsales <- auto.arima(salesarima)
fitlnsales
plot(salesarima , type = 'l')
exp(lnstock)

forecastevalues_ln <- forecast(fitlnsales , h =12)
forecastevalues_ln
plot(forecastevalues_ln)

forecastedextract <- as.numeric(forecastevalues_ln$mean)
finalvalues <- exp(forecastedextract)
finalvalues

df <- data.frame(Pepperfry$Week.Sales[51:62],finalvalues)
col_headings <- c("Actual Price","Forecasted Price")
names(df)<-col_headings
attach(df)
mape <- ((df$`Actual Price`-df$`Forecasted Price`)/(df$`Actual Price`))
mape
mean(mape)






