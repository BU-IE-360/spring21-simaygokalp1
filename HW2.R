library(readxl)
library(data.table)
library(zoo)
library(GGally)
library(colorspace)
library(ggplot2)
library(skimr)
library(ggcorrplot)
library(car)

Liquidated <- read_excel("Liquidated.xlsx")
Liquidated <- as.data.table(Liquidated)
names(Liquidated) = c("date", "liquidated")
Liquidated$date <- as.Date(as.yearmon(Liquidated$date))
Liquidated <- Liquidated[order(as.Date(Liquidated$date)),]
str(Liquidated)

dolar <- read_excel("dolar.xlsx")
dolar <- as.data.table(dolar)
dolar$date <- as.Date(as.yearmon(dolar$date))
str(dolar)

interest_rate <- read_excel("interest_rate.xlsx")
interest_rate<- as.data.table(interest_rate)
interest_rate$date <- as.Date(as.yearmon(interest_rate$date))
str(interest_rate)


Liq_Money <- read_excel("Liq_Money.xlsx")
Liq_Money<- as.data.table(Liq_Money)
names(Liq_Money) = c("date", "liq_money")
Liq_Money$date <- as.Date(as.yearmon(Liq_Money$date))
Liq_Money <- Liq_Money[order(as.Date(Liq_Money$date)),]
str(Liq_Money)


price_index <- read_excel("price_index.xlsx")
price_index  <- as.data.table(price_index)
names(price_index) = c("date", "price_index")
price_index$date <- as.Date(as.yearmon(price_index$date))
str(price_index)

opened <- read_excel("opened.xlsx")
opened  <- as.data.table(opened)
names(opened) = c("date", "opened")
opened$date <- as.Date(as.yearmon(opened$date))
str(opened)

all_data <- Liquidated
all_data <- all_data[,dolar:=dolar$dolar[1:135]]
all_data <- all_data[,interest:=interest_rate$interest[1:135]]
all_data <- all_data[,liq_money:=Liq_Money$liq_money[1:135]]
all_data <- all_data[,opened:=opened$opened[1:135]]
all_data <- all_data[,price_index:=price_index$price_index[1:135]]


summary_data=skim(all_data)
print(summary_data)

ggplot(all_data, aes(x=date, y=liquidated)) + geom_line() + stat_smooth() +
  
  xlab("") + scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme(axis.text.x = element_text(size = 18)) + theme(axis.text.y = element_text(size = 18)) +
  xlab("Date") + ylab("Number of Liquidated Companies") + theme(axis.title.x = element_text(size = 25)) + theme(axis.title.y=element_text(size = 25)) + ggtitle("Trend of Liquidated Total (Number)") + theme(title = element_text(size = 30))


ggplot(all_data, aes(x=liquidated)) + geom_histogram() + xlab("Count") + ylab("Number of Liquidated Companies") + ggtitle("Histogram of Liquidated Total (Number)") + theme(title = element_text(size = 30)) + theme(axis.title.x = element_text(size = 25)) + theme(axis.title.y=element_text(size = 25)) + theme(axis.text.x = element_text(size = 18)) + theme(axis.text.y = element_text(size = 18))

liq_log <- log(all_data$liquidated)

all_data=all_data[,log_liq:=as.numeric(liq_log)]

ggplot(all_data, aes(x=log_liq)) + geom_histogram() + xlab("Count") + ylab("Number of Log of Liquidated Companies") + ggtitle("Histogram of Log of Liquidated Total (Number)") + theme(title = element_text(size = 30)) + theme(axis.title.x = element_text(size = 25)) + theme(axis.title.y=element_text(size = 25)) + theme(axis.text.x = element_text(size = 18)) + theme(axis.text.y = element_text(size = 18))

qqPlot(all_data$liquidated , ylab = "Liquidated Total (Number)", xlab = "Theoretical Quantities", main = "Q-Q Plot of Liquidated Total (Number)") 

qqPlot(all_data$log_liq, ylab = "Log of Liquidated Total (Number)" , xlab = "Theoretical Quantities", main= "Q-Q Plot of Log of Liquidated Total (Number)")

ggplot(all_data, aes(x=date, y=log_liq)) + geom_line() + stat_smooth() +
  
  xlab("") + scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme(axis.text.x = element_text(size = 18)) + theme(axis.text.y = element_text(size = 18)) +
  xlab("Date") + ylab("Number of Log of Liquidated Companies") + theme(axis.title.x = element_text(size = 25)) + theme(axis.title.y=element_text(size = 25)) + ggtitle("Trend of Log of Liquidated Total (Number)") + theme(title = element_text(size = 30))


acf(all_data$log_liq, main = "Log of Liquidated Total (Number) ACF")

month = seq(1,12,by=1)

all_data = cbind(all_data,month)

all_data <- all_data[,trend:=1:.N]
ts_all_data <- ts(all_data, frequency = 12, start = c(2010,1))

numeric_data <- all_data
numeric_data$date = NULL
numeric_data$liquidated = NULL


correl_info =cor(numeric_data)
ggcorrplot(correl_info, hc.order = TRUE, type = "lower",lab = TRUE)

ggpairs(numeric_data)

model1_data <- numeric_data
model1_data$month = as.factor(model1_data$month)

model=lm(log_liq~.,model1_data)
summary(model)


model1_data[,trend_constant:=predict(model,model1_data)]
model1_data[,date:=all_data$date]

ggplot(model1_data, aes(x=date)) + geom_line(aes(y=log_liq)) + geom_line(aes(y=trend_constant,color='trend')) + geom_line(aes(y=log_liq,color='Liquidated Total (Number)')) + xlab("Date") + ylab("Trend and Actual Data") + ggtitle("Trend vs Actual Data Time Series for Initial Model") + theme(title = element_text(size = 30)) + theme(axis.title.x = element_text(size = 25)) + theme(axis.title.y=element_text(size = 25)) + theme(axis.text.x = element_text(size = 18)) + theme(axis.text.y = element_text(size = 18))  + theme(
  legend.title = element_text(size = 20),
  legend.text = element_text(size = 20)
)

model1=lm(log_liq~opened + month + trend, model1_data)
summary(model1)

model1_data[,trend_constant1:=predict(model1,model1_data)]
model1_data[,date:=all_data$date]

ggplot(model1_data, aes(x=date)) + geom_line(aes(y=log_liq)) + geom_line(aes(y=trend_constant1,color='trend')) + geom_line(aes(y=log_liq,color='Liquidated Total (Number)')) + xlab("Date") + ylab("Trend and Actual Data") + ggtitle("Trend vs Actual Data Time Series for Second Model") + theme(title = element_text(size = 30)) + theme(axis.title.x = element_text(size = 25)) + theme(axis.title.y=element_text(size = 25)) + theme(axis.text.x = element_text(size = 18)) + theme(axis.text.y = element_text(size = 18))  + theme(
  legend.title = element_text(size = 20),
  legend.text = element_text(size = 20)
)


checkresiduals(model1)
mean(model1$residuals)


model1_data[,fitted:=fitted(model1)]
model1_data[,residual:=residuals(model1)]

ggplot(model1_data, aes(x=fitted, y=residual)) + 
  geom_point() + ggtitle("Residuals vs Fitted Value") + theme(title = element_text(size = 30)) + theme(axis.title.x = element_text(size = 25)) + theme(axis.title.y=element_text(size = 25)) + theme(axis.text.x = element_text(size = 18)) + theme(axis.text.y = element_text(size = 18))

ggplot(model1_data, aes(x=fitted, y=log_liq)) + 
  geom_point() + geom_abline(slope=1, intercept=0) +
  ylab("Target Value") + ggtitle("Target Value vs Fitted Value") + theme(title = element_text(size = 30)) + theme(axis.title.x = element_text(size = 25)) + theme(axis.title.y=element_text(size = 25)) + theme(axis.text.x = element_text(size = 18)) + theme(axis.text.y = element_text(size = 18))





