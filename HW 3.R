library(readr)
library(data.table)
library(ggplot2)
library(urca)
library(zoo)

data <- read_csv("GercekZamanliTuketim-01012016-20052021.csv")
data$Tarih <- format(as.Date(data$Tarih, format = "%d.%m.%Y"), "%Y-%m-%d")
data$Tarih <- as.Date(data$Tarih)
str(data$Tarih)
names(data) <- c("date","hour","cons")

ggplot(data, aes(x=date, y=cons)) + geom_line() + stat_smooth() +
  
  xlab("") + scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme(axis.text.x = element_text(size = 15)) + theme(axis.text.y = element_text(size = 15)) +
  xlab("Date") + ylab("Daily Consumption") + theme(axis.title.x = element_text(size = 20)) + theme(axis.title.y=element_text(size = 20)) + ggtitle("Trend of Daily Consumption") + theme(title = element_text(size = 25))


data <- as.data.table(data)
data <- data[,trend:=1:.N]

data_first <- data[1:720,]

ggplot(data_first, aes(x=trend, y=cons)) + geom_line() + stat_smooth() +   theme(axis.text.x = element_text(size = 18)) + theme(axis.text.y = element_text(size = 18)) +
  xlab("Hour Index") + ylab("Hourly Consumption") + theme(axis.title.x = element_text(size = 25)) + theme(axis.title.y=element_text(size = 25)) + ggtitle("Trend of Hourly Consumption for a Month") + theme(title = element_text(size = 30))


data_first_2 <- data[1:72,]

ggplot(data_first_2, aes(x=trend, y=cons)) + geom_line() + stat_smooth() + scale_x_continuous("trend", labels = as.character(data_first_2$trend), breaks = data_first_2$trend) +   theme(axis.text.x = element_text(size = 10)) + theme(axis.text.y = element_text(size = 18)) +
  xlab("Hour Index") + ylab("Hourly Consumption") + theme(axis.title.x = element_text(size = 25)) + theme(axis.title.y=element_text(size = 25)) + ggtitle("Trend of Hourly Consumption for 3 Days") + theme(title = element_text(size = 30))


acf(data$cons, main= "Hourly Autocorrelation")

pacf(data$cons, main = "Hourly Partial Autocorrelation")

datats<-ts(data$cons,freq=24)


data_mult<-decompose(datats,type="multiplicative")
random=data_mult$random
plot(data_mult)


plot(data_mult$seasonal[1:24], xlab = "Hour", ylab = "Multiplicative of Hour", main = "Seasonal Component of Trend for Multiplicative")


unt_test=ur.kpss(data_mult$random) 
summary(unt_test)


data_add<-decompose(datats,type="additive")
random=data_add$random
plot(data_add)


plot(data_add$seasonal[1:24], xlab = "Hour", ylab = "Additive Component of Hour", main = "Seasonal Component of Trend for Additive Model")


unt_test=ur.kpss(data_add$random) 
summary(unt_test)


daily_consumption_sum=data[,list(sum_consumption = sum(cons,na.rm=T)), by=list(date)]

daily_consumption=data[,list(mean_consumption = mean(cons,na.rm=T)), by=list(date)]



ggplot(daily_consumption, aes(x=date, y=mean_consumption)) + geom_line() + stat_smooth() +   theme(axis.text.x = element_text(size = 18)) + theme(axis.text.y = element_text(size = 18)) +
  xlab("Date") + ylab("Daily Consumption") + theme(axis.title.x = element_text(size = 25)) + theme(axis.title.y=element_text(size = 25)) + ggtitle("Trend of Daily Consumption") + theme(title = element_text(size = 30))

daily_consumption_first <- daily_consumption[1:90,]

ggplot(daily_consumption_first, aes(x=date, y=mean_consumption)) + geom_line() + stat_smooth() +   theme(axis.text.x = element_text(size = 18)) + theme(axis.text.y = element_text(size = 18)) +
  xlab("Date") + ylab("Daily Consumption") + theme(axis.title.x = element_text(size = 25)) + theme(axis.title.y=element_text(size = 25)) + ggtitle("Trend of Daily Consumption for 3 months") + theme(title = element_text(size = 30))


acf(daily_consumption$mean_consumption, main= "Daily Autocorrelation")

pacf(daily_consumption$mean_consumption, main= "Daily Partial Autocorrelation")

data_daily_ts<-ts(daily_consumption$mean_consumption,freq=7)

data_daily_mult<-decompose(data_daily_ts,type="multiplicative")
random=data_daily_mult$random
plot(data_daily_mult)


plot(data_daily_mult$seasonal[1:7], xlab = "Hour", ylab = "Multiplicative of Hour", main = "Seasonal Component of Trend for Multiplicative")

unt_test=ur.kpss(data_daily_mult$random) 
summary(unt_test)


data_daily_add<-decompose(data_daily_ts,type="additive")
random=data_daily_add$random
plot(data_daily_add)


plot(data_daily_add$seasonal[1:7], xlab = "Hour", ylab = "Multiplicative of Hour", main = "Seasonal Component of Trend for Multiplicative")

unt_test=ur.kpss(data_daily_add$random) 
summary(unt_test)



data[,month_date:=format(as.Date(data$date,  format = "%d-%m-%Y"), "%Y-%m")]

mon_consumption=data[,list(mean_consumption = mean(cons,na.rm=T)), by="month_date"]

mon_consumption$month_date = as.yearmon(mon_consumption$month_date)

ggplot(mon_consumption, aes(x=month_date, y=mean_consumption)) + geom_line() + stat_smooth() +   theme(axis.text.x = element_text(size = 18)) + theme(axis.text.y = element_text(size = 18)) +
  xlab("Date") + ylab("Monthly Consumption") + theme(axis.title.x = element_text(size = 25)) + theme(axis.title.y=element_text(size = 25)) + ggtitle("Trend of Mothly Consumption") + theme(title = element_text(size = 30))

acf(mon_consumption$mean_consumption, main= "Monthly Autocorrelation")

pacf(mon_consumption$mean_consumption, main= "Monthly Partial Autocorrelation")



data_mon_ts<-ts(mon_consumption$mean_consumption,freq=12, start=c(2016,1))

data_mon_mult<-decompose(data_mon_ts,type="multiplicative")
random=data_mon_mult$random
plot(data_mon_mult)

plot(data_mon_mult$seasonal[1:12], xlab = "Month", ylab = "Multiplicative Component of Month", main = "Seasonal Component of Trend for Multiplicative Model")


unt_test=ur.kpss(data_mon_mult$random) 
summary(unt_test)

data_mon_add<-decompose(data_mon_ts,type="additive")
random=data_mon_add$random
plot(data_mon_add)

plot(data_mon_add$seasonal[1:12], xlab = "Month", ylab = "Additive Component of Month", main = "Seasonal Component of Trend for Additive Model")


unt_test=ur.kpss(data_mon_add$random) 
summary(unt_test)

datats_2<-ts(data$cons,freq=168)

data_mult_2<-decompose(datats_2,type="multiplicative")
random=data_mult_2$random
plot(data_mult_2)

plot(data_mult_2$seasonal[1:168], xlab = "Hour", ylab = "Multiplicative of Hour & Week", main = "Seasonal Component of Trend for Multiplicative")

unt_test=ur.kpss(data_mult_2$random) 
summary(unt_test)

data_add_2<-decompose(datats_2,type="additive")
random_add_final=data_add_2$random
plot(data_add_2)

plot(data_add_2$seasonal[1:168], xlab = "Hour", ylab = "Additive Component of Hour", main = "Seasonal Component of Trend for Additive Model")

unt_test=ur.kpss(data_add_2$random) 
summary(unt_test)


plot(random_add_final, xlab = "Hour Index", ylab = "Consumption Random Part", main = "Random Data Trend")

acf(random_add_final, na.action = na.pass, main= "Detrend's Autocorrelation")

pacf(random_add_final, na.action = na.pass, main = "Detrend's Partial Autocorrelation")

library(stats)
model <- arima(random_add_final, order=c(10,0,0))
print(model)
AIC(model)
BIC(model)

model <- arima(random_add_final, order=c(1,0,0))
print(model)
AIC(model)
BIC(model)

model <- arima(random_add_final, order=c(11,0,0))
print(model)
AIC(model)
BIC(model)

model <- arima(random_add_final, order=c(9,0,0))
print(model)
AIC(model)
BIC(model)

acf(random_add_final, na.action = na.pass, main= "Detrend's Autocorrelation")
pacf(random_add_final, na.action = na.pass, main = "Detrend's Partial Autocorrelation")

model <- arima(random_add_final, order=c(0,0,2))
print(model)
AIC(model)
BIC(model)

model <- arima(random_add_final, order=c(0,0,10))
print(model)
AIC(model)
BIC(model)

model <- arima(random_add_final, order=c(0,0,11))
print(model)
AIC(model)
BIC(model)

model <- arima(random_add_final, order=c(0,0,12))
print(model)
AIC(model)
BIC(model)

model <- arima(random_add_final, order=c(10,0,12))
print(model)
AIC(model)
BIC(model)

model <- arima(random_add_final, order=c(1,0,1))
print(model)
AIC(model)
BIC(model)

model_final <- arima(random_add_final, order=c(10,0,12))
print(model_final)
AIC(model_final)
BIC(model_final)


model_fitted <- random_add_final - residuals(model_final)


detrend = cbind(random_add_final, residuals(model_final))
detrend = as.data.table(detrend)
detrend = detrend[,date:= data$date]
names(detrend) = c("detrend","prediction","date")

ggplot(detrend, aes(x=date)) + geom_line(aes(y=detrend)) + 
  geom_line(aes(y=detrend,color='detrend actual')) + 
  geom_line(aes(y=prediction,color='predicted detrend')) + 
  xlab("Date") + ylab("ARIMA Prediction and Actual Detrend Data") + ggtitle("ARIMA vs Actual Detrend Data Time Series for Second Model") + theme(title = element_text(size = 30)) + theme(axis.title.x = element_text(size = 25)) + theme(axis.title.y=element_text(size = 25)) + theme(axis.text.x = element_text(size = 18)) + theme(axis.text.y = element_text(size = 18))  + theme(
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20)
  )

detrend = detrend[,model_prediction:= data_add_2$trend + data_add_2$seasonal + prediction]
detrend = detrend[,actual:= data$cons]

ggplot(detrend, aes(x=date))  + 
  geom_line(aes(y=actual,color='consumption actual')) + 
  geom_line(aes(y=model_prediction,color='predicted consumption')) + 
  xlab("Date") + ylab("ARIMA Prediction and Actual Consumption Data") + ggtitle("ARIMA vs Actual Consumption Data Time Series") + theme(title = element_text(size = 30)) + theme(axis.title.x = element_text(size = 25)) + theme(axis.title.y=element_text(size = 25)) + theme(axis.text.x = element_text(size = 18)) + theme(axis.text.y = element_text(size = 18))  + theme(
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20)
  )

model_forecast <- predict(model_final, n.ahead = 84)$pred


last_trend <- tail(data_add_2$trend[!is.na(data_add_2$trend)],1)

detrend = detrend[,last_trend := as.numeric(last_trend)]
detrend = detrend[,seasonality := as.numeric(data_add_2$seasonal)]

prediction_needed = detrend[47125:.N,]  
model_forecast <- predict(model_final, n.ahead = 84)$pred

prediction_needed = prediction_needed[,prediction := predict(model_final, n.ahead = 84)$pred]

prediction_needed = prediction_needed[,last_forecast := prediction + last_trend + seasonality]

detrend$model_prediction[47125:47208]  <- prediction_needed$last_forecast

ggplot(detrend, aes(x=date))  + 
  geom_line(aes(y=actual,color='consumption actual')) + 
  geom_line(aes(y=model_prediction,color='predicted consumption')) + 
  xlab("Date") + ylab("ARIMA Prediction and Actual Consumption Data") + ggtitle("ARIMA vs Actual Consumption Data Time Series with Predictions") + theme(title = element_text(size = 30)) + theme(axis.title.x = element_text(size = 25)) + theme(axis.title.y=element_text(size = 25)) + theme(axis.text.x = element_text(size = 18)) + theme(axis.text.y = element_text(size = 18))  + theme(
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20)
  )

daily_actual=detrend[,list(mean_consumption_actual = mean(actual,na.rm=T)), by=list(date)]
daily_prediction=detrend[,list(mean_consumption_pred = mean(model_prediction,na.rm=T)), by=list(date)]

daily_actual = daily_actual[,daily_prediction := daily_prediction$mean_consumption_pred]

daily_actual_last = tail(daily_actual,15)

daily_actual_last$daily_prediction <- as.numeric(daily_actual_last$daily_prediction)

str(daily_actual_last)


ggplot(daily_actual, aes(x=date))  + 
  geom_line(aes(y=mean_consumption_actual,color='consumption actual')) + 
  geom_line(aes(y=daily_prediction,color='predicted consumption')) + 
  xlab("Date") + ylab("ARIMA Prediction and Actual Consumption Data") + ggtitle("ARIMA vs Actual Daily Consumption Data Time Series with Predictions") + theme(title = element_text(size = 30)) + theme(axis.title.x = element_text(size = 25)) + theme(axis.title.y=element_text(size = 25)) + theme(axis.text.x = element_text(size = 18)) + theme(axis.text.y = element_text(size = 18))  + theme(
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20)
  )

ggplot(daily_actual_last, aes(x=date))  + 
  geom_line(aes(y=mean_consumption_actual,color='consumption actual')) + 
  geom_line(aes(y=daily_prediction,color='predicted consumption')) + 
  xlab("Date") + ylab("ARIMA Prediction and Actual Consumption Data") + ggtitle("ARIMA vs Actual Daily Consumption Data Time Series with Predictions for 15 Days") + theme(title = element_text(size = 30)) + theme(axis.title.x = element_text(size = 25)) + theme(axis.title.y=element_text(size = 25)) + theme(axis.text.x = element_text(size = 18)) + theme(axis.text.y = element_text(size = 18))  + theme(
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20)
  )

accu=function(actual,forecast){
  n=length(actual)
  error=actual-forecast
  mean=mean(actual)
  sd=sd(actual)
  CV=sd/mean
  FBias=sum(error)/sum(actual)
  MAPE=sum(abs(error/actual))/n
  RMSE=sqrt(sum(error^2)/n)
  MAD=sum(abs(error))/n
  MADP=sum(abs(error))/sum(abs(actual))
  WMAPE=MAD/mean
  l=data.frame(n,mean,sd,CV,FBias,MAPE,RMSE,MAD,MADP,WMAPE)
  return(l)
}

accu(daily_actual_last$mean_consumption_actual, daily_actual_last$daily_prediction)






