install.packages("fpp2")
library(fpp2)
install.packages("forecast")
library(forecast)
library(tidyr)
library(dplyr)

# Initial data processing -------------------------------------------------

# Data load in
clim <- read.csv("G:/My Drive/Everything/Belgeler/RDirectory/g_bronx_processed_temp.csv", row.names=NULL)
clim <- dplyr::filter(clim, clim$stn==725030)
pol <- read.csv("G:/My Drive/Everything/Belgeler/RDirectory/g_bronx_processed_ozone.csv")
pol$date <- as.Date(pol$Date)
clim$date <- as.Date(clim$Date)
# Process the data
#colnames(clim) <- c("index", "year","mo","da","temp","dewp","visib","wdsp","prcp","fog","rain_drizzle","snow_ice_pellets","hail","thunder","tornado_funnel_cloud")
#colnames(pol) <- c("date", "source","siteID","poc","o3","unit","airquality","sitename","obs","completion%","airquality code","airqualitycodedescp","CBSA","area","statecode","state","county#","county","latitude","longitude")
n <- length(clim$temp)
npol <- length(pol$Daily.Max.8.hour.Ozone.Concentration)
#clim$date <- base::as.Date(paste(clim$mo[1:n], clim$da[1:n], clim$year[1:n], sep = "/"), format = "%m/%d/%Y")
#pol$date <- as.Date(paste(substr(as.character(pol$Date),1,2)
#                          , substr(as.character(pol$Date),4,5)
#                          , substr(as.character(pol$Date),7,10)
#                          , sep = "-"),format ="%m-%d-%Y")

## Aligning dates and ordering
min(pol$d)
climt <- dplyr::filter(clim, clim$date >= min(pol$date))
climt <- climt[order(climt$date),]
polt <- dplyr::filter(pol, pol$Date <= max(climt$date))
polt <- polt[order(polt$Date),]
df<- data.frame(climt,polt)
## With hand specified


# Train-test split --------------------------------------------------------


train <- dplyr::filter(df,df$date<"2020-03-20")
test <- dplyr::filter(df,df$date>="2020-03-20")
traintrain <- dplyr::filter(df,df$date<="2019-08-14")
traintest <- dplyr::filter(df,df$date>"2019-08-14")
head(train,2)

# Trying out different models --------------------------------------------


m1<-Arima(train$temp, order = c(5,1,1), seasonal = c(c(0,1,1),365),include.constant = TRUE)
m2<-Arima(train$temp, seasonal = c(c(0,1,0),365),include.constant = TRUE)
m3<-Arima(train$temp, order = c(0,1,0), include.constant = TRUE)
m4<-auto.arima(train$temp, d = 1,D = 1,start.q = 1,start.Q = 1, seasonal=TRUE,max.p = 20)
summary(m4)
m5<-auto.arima(train2$climt_deseasonal)
m6<-auto.arima(train$temp)
m7<-auto.arima(train$temp, xreg = train$Daily.Max.8.hour.Ozone.Concentration)
m8<-Arima(train$temp,order=c(9,1,1),seasonal = c(c(9,1,1),c(0,1,1),365))
m9<-Arima(train$temp,order=c(9,1,1),seasonal = c(c(9,1,1),c(0,1,1),365))
m10<-Arima(train$temp,order=c(9,1,1),seasonal = c(c(9,1,1),c(0,1,1),365),xreg = train$Daily.Max.8.hour.Ozone.Concentration)

accuracy(m8)
accuracy(m9)

AIC(m6,m8,m7,m9,m10)
par(mfrow=c(1,2))
checkresiduals(m1)
checkresiduals(m2)
checkresiduals(m3)
checkresiduals(m4)
checkresiduals(m6,lag = 365)
checkresiduals(m8)
checkresiduals(m9)
pacf(train$temp)


# plot models ^ -----------------------------------------------------------



m1df <- summary(forecast(m8,level = c(80,95),h = length(test$date)))
m1df <- m1df[1:60,]
par(mfrow=c(2,2))
plot(train$date,train$temp,pch=16,cex=.4,col="blue",type="l",xlim=c(min(train$date),max(test$date)))
points(x=test$date,y=m1df$`Point Forecast`,col="red",cex=.4,type="l")
points(x=test$date,y=test$temp,col="green",cex=.4,type="l")
lines(x=test$date,y=m1df$`Lo 95`,col="red",cex=.4,type="l")
lines(x=test$date,y=m1df$`Hi 95`,col="red",cex=.4,type="l")
#closeup
plot(train$date,train$temp,pch=16,cex=.4,col="blue",type="l",xlim=c(min(tail(train$date,180)),tail(max(test$date),180)))
points(x=test$date,y=m1df$`Point Forecast`,col="red",cex=.4,type="l")
points(x=test$date,y=test$temp,col="green",cex=.4,type="l")
lines(x=test$date,y=m1df$`Lo 95`,col="red",cex=.4,type="l")
lines(x=test$date,y=m1df$`Hi 95`,col="red",cex=.4,type="l")

m2df <- summary(forecast(m9,level = c(80,95),h = length(test$date),xreg = train$Daily.Max.8.hour.Ozone.Concentration))
m2df <- m2df[1:60,]

#whole range
plot(train$date,train$temp,pch=16,cex=.4,col="blue",type="l",xlim=c(min(train$date),max(test$date)))
points(x=test$date,y=m2df$`Point Forecast`,col="red",cex=.4,type="l")
points(x=test$date,y=test$temp,col="green",cex=.4,type="l")
lines(x=test$date,y=m2df$`Lo 95`,col="red",cex=.4,type="l")
lines(x=test$date,y=m2df$`Hi 95`,col="red",cex=.4,type="l")
#closeup
plot(train$date,train$temp,pch=16,cex=.4,col="blue",type="l",xlim=c(min(tail(train$date,180)),tail(max(test$date),180)))
points(x=test$date,y=m2df$`Point Forecast`,col="red",cex=.4,type="l")
points(x=test$date,y=test$temp,col="green",cex=.4,type="l")
lines(x=test$date,y=m2df$`Lo 95`,col="red",cex=.4,type="l")
lines(x=test$date,y=m2df$`Hi 95`,col="red",cex=.4,type="l")


# change param ------------------------------------------------------------

p = 9
d = 1
q = 10

# model selection bit -----------------------------------------------------

m8<-Arima(train$temp,order=c(p,d,q),seasonal = c(c(p,d,q),c(0,1,1),365))
m10<-Arima(train$temp,order=c(p,d,q),seasonal = c(c(p,d,q),c(0,1,1),365),xreg = train$Daily.Max.8.hour.Ozone.Concentration)
#m11<-Arima(train$temp,order=c(p,d,q),seasonal = c(c(p,d,q),c(0,1,1),365),xreg = train$Daily.Max.8.hour.Ozone.Concentration)

AIC(m8,m10)
BIC(m8,m10)
#checkresiduals(m8)
#checkresiduals(m10)
#pacf(train$temp)
which(is.na(train$temp))


m1df <- summary(forecast(m8,level = c(80,95),h = length(test$date)))
m1df <- m1df[1:60,]
par(mfrow=c(2,2))
title="without regressor"
plot(train$date,train$temp,pch=16,cex=.4,col="blue",type="l",main=title,xlim=c(min(train$date),max(test$date)))
points(x=test$date,y=m1df$`Point Forecast`,col="red",cex=.4,type="l")
points(x=test$date,y=test$temp,col="green",cex=.4,type="l")
lines(x=test$date,y=m1df$`Lo 95`,col="red",cex=.4,type="l")
lines(x=test$date,y=m1df$`Hi 95`,col="red",cex=.4,type="l")
#closeup
plot(train$date,train$temp,pch=16,cex=.4,col="blue",type="l",main=title,xlim=c(min(tail(train$date,180)),tail(max(test$date),180)))
points(x=test$date,y=m1df$`Point Forecast`,col="red",cex=.4,type="l")
points(x=test$date,y=test$temp,col="green",cex=.4,type="l")
lines(x=test$date,y=m1df$`Lo 95`,col="red",cex=.4,type="l")
lines(x=test$date,y=m1df$`Hi 95`,col="red",cex=.4,type="l")

#forecast_without_regressor <- write.csv(m1df)

m2df <- summary(forecast(m10,level = c(80,95),h = length(test$date),xreg = test$Daily.Max.8.hour.Ozone.Concentration))
m2df <- m2df[1:60,]
title<-"with regressor"
#whole range
plot(train$date,train$temp,pch=16,cex=.4,col="blue",type="l",main=title,xlim=c(min(train$date),max(test$date)))
points(x=test$date,y=m2df$`Point Forecast`,col="red",cex=.4,type="l")
points(x=test$date,y=test$temp,col="green",cex=.4,type="l")
lines(x=test$date,y=m2df$`Lo 95`,col="red",cex=.4,type="l")
lines(x=test$date,y=m2df$`Hi 95`,col="red",cex=.4,type="l")
#closeup
plot(train$date,train$temp,pch=16,cex=.4,col="blue",type="l",main=title,xlim=c(min(tail(train$date,180)),tail(max(test$date),180)))
points(x=test$date,y=m2df$`Point Forecast`,col="red",cex=.4,type="l")
points(x=test$date,y=test$temp,col="green",cex=.4,type="l")
lines(x=test$date,y=m2df$`Lo 95`,col="red",cex=.4,type="l")
lines(x=test$date,y=m2df$`Hi 95`,col="red",cex=.4,type="l")

write.csv(m1df,"forecast_without_regressor.csv")
write.csv(m2df,"forecast_with_regressor.csv")

file.create(forecast_with_regressor, filename="ForecastwRegressor")
AIC(m8,m10)
summary(m10)


# the rest ----------------------------------------------------------------



## Looking at residuals of the arima model with no exogenous, and the pollution data
scatter.smooth(train$Daily.Max.8.hour.Ozone.Concentration,m6$residuals, pch=16,cex=.4)
## it looks like we can use the ozone information to improve our model

## Automated method
## Deseasonalizing temp and pol data first
tseries = ts(na.omit(climt$temp), frequency=365) 
climt_decomp = stl(tseries, s.window="periodic")
climt_deseasonal <- seasadj(decomp)
plot(climt_decomp)
plot(climt$date,climt_deseasonal,pch=16,cex=.4,type="l")
plot(tail(climt$date,1000),tail(climt_deseasonal,1000),pch=16,cex=.4,type="l")


polt_tseries = ts(na.omit(polt$Daily.Max.8.hour.Ozone.Concentration), frequency=365)
polt_decomp <- stl(polt_tseries,s.window="periodic")
polt_deseasonal <- seasadj(polt_decomp)
plot(polt_decomp)
plot(climt$date,polt_deseasonal,pch=16,cex=.4,type="l")

df_ds <- data.frame(date=climt$date,climt_deseasonal,polt_deseasonal)

## Train - test split
train2 <- dplyr::filter(df_ds,climt$date<"2020-03-20")
test2 <- dplyr::filter(df_ds,climt$date>="2020-03-20")
traintrain2 <- dplyr::filter(df_ds,climt$date<="2019-08-14")
traintest2 <- dplyr::filter(df_ds,climt$date>"2019-08-14")

## Fitting model with only temperature
arima1 <- forecast::auto.arima(train$climt_deseasonal)
#checkresiduals(arima1)
a1 <- forecast(arima1,level = c(80,95))
df1 <- summary(a1)
df1 <- df[1:60,]

#whole range
par(mfrow=c(2,2))
plot(train$date,train$climt_deseasonal,pch=16,cex=.4,col="blue",type="l",xlim=c(min(train$date),max(test$date)))
points(x=test$date,y=df1$`Point Forecast`,col="red",cex=.4,type="l")
points(x=test$date,y=test$climt_deseasonal,col="green",cex=.4,type="l")
#closeup
plot(tail(train$date,180),tail(train$climt_deseasonal,180),pch=16,cex=.4,col="blue",type="l",xlim = c(min(tail(train$date,180)),max(test$date)))
points(x=test$date,y=df1$`Point Forecast`,col="red",cex=.4,type="l")
points(x=test$date,y=test$climt_deseasonal,col="green",cex=.4,type="l")

## Fitting model with exogenous
arima2 <- forecast::auto.arima(train$climt_deseasonal,xreg =train$polt_deseasonal)
#checkresiduals(arima2)
a2 <- forecast(arima2),level = c(80,95), xreg=train$polt_deseasonal)
df2 <- summary(a)
df2 <- df2[1:60,]
#whole range
plot(train$date,train$climt_deseasonal,pch=16,cex=.4,col="blue",type="l")
points(x=test$date,y=df2$`Point Forecast`,col="red",cex=.4,type="l")
points(x=test$date,y=test$climt_deseasonal,col="green",cex=.4,type="l")
#closeup
plot(tail(train$date,180),tail(train$climt_deseasonal,180),pch=16,cex=.4,col="blue",type="l",xlim = c(min(tail(train$date,180)),max(test$date)))
points(x=test$date,y=df2$`Point Forecast`,col="red",cex=.4,type="l")
points(x=test$date,y=test$climt_deseasonal,col="green",cex=.4,type="l")




acf(deseasonal, lag.max = 365)
# both acf() and pacf() generates plots by default
acfRes <- acf(clim$temp, lag.max = 365*1) # autocorrelation
pacfRes <- pacf(clim$temp, lag.max = 365*1)  # partial autocorrelation
ccfRes <- ccf(clim$temp, clim$pol, ylab = "cross-correlation", lag.max = 365*1) # computes cross correlation between 2 timeseries.

class(decomp$time.series)
climt_remainder<-decomp$time.series[,3]
polt_remainder<-pol_decomp$time.series[,3]
plot(polt_remainder,climt_remainder,pch=16,cex=.4, main="Residual Plot", xlab="Residuals from")



##manual differencing the data if necessarry
diff<-c()
lag<-365
for(i in 1:n){
  dif[n+1-i] <- clim$temp[n+1-i]-clim$temp[n+1-i-lag]
  diff <- append(diff,dif[n+1-i])
}
acf(diff, lag.max = 365)

## side project for later
plist <- c()
for(i in (365*3):1500){
  a<- adf.test(tail(clim$temp,i),k = 365)
  append(plist,a$p.value)
}

plot(plist)
adf.test()





## How temperature changed over the years
par(mfrow=c(2,2))
plot(clim$date,clim$temp, pch=16, cex=.1, type="p",xlab="",ylab="Average Temp (F)")
plot(clim$date[which(clim$year>2000)],clim$temp[which(clim$year>2000)], pch=16, cex=.1, type="p",xlab="",ylab="Average Temp (F)")
plot(clim$date[which(clim$year>2010)],clim$temp[which(clim$year>2010)], pch=16, cex=.1, type="p",xlab="",ylab="Average Temp (F)")
plot(clim$date[which(clim$year>2015)],clim$temp[which(clim$year>2015)], pch=16, cex=.1, type="p",xlab="",ylab="Average Temp (F)")

## The cycle seems to be fairly annual.
## Now let's use the seasonal naive method to predict 
## the temperature of april and may
clim_train <- clim[which(clim$date<="2020-03-18"),]
clim_test <- clim[which(clim$date>"2020-03-18"),]

# Getting the predicted dates from a year ago: THIS IS THE SEASONAL NAIVE METHOD
# meaning cycle = 365
cycle = 365*1
predicted_temp <- c()
prediction_dates <- clim_test$date - cycle

for(i in 1:length(prediction_dates)){
  pred<-clim_train$temp[which(clim_train$date == prediction_dates[i])]
  predicted_temp <- rbind(predicted_temp,pred)
}
predicted_temp<-predicted_temp[1:length(predicted_temp)]
predicted_temp
## Now let's plot the seasonal naive
## predicted temperatures against
## the observed temperatures
par(mfrow=c(1,1))
plot(clim_train$date[which(clim_train$date>"2019-03-18")],clim_train$temp[which(clim_train$date>"2019-03-18")], pch=16, cex=.5,
     xlab='Dates',ylab='Temperature in F', col='blue', xlim=as.Date(c("2019-03-18", "2020-05-18")))
points(clim_test$date,clim_test$temp,pch=16, cex=.5,col='red')
points(clim_test$date,predicted_temp,pch=16,cex=.5,col='green')

### Polynomial line fitted with real data beginning from
### the first day of the lockdown
date<-1:length(clim_test$date)

lo <- loess(clim_test$temp~date, span=.5, degree=2,parametric = "clim_test$temp")
df<-predict(lo,se=TRUE)

plot(date,clim_test$temp,pch=16, cex=1, col="black",xlab="Days since quarantine")
lines(df$fit, col='red', lwd=2)
lines(df$fit-df$se.fit,col="red", lty=2)
lines(df$fit+df$se.fit,col="red", lty=2)

### Polynomial line fitted for the same period but using
### the predicted values using the seasonal naive method
lo2 <- loess(predicted_temp~date, span=.5, degree=2,parametric = "predicted_temp")
df2<-predict(lo2,se=TRUE)
lines(df2$fit, col='green', lwd=2)
lines(df2$fit-df$se.fit,col="green", lty=2)
lines(df2$fit+df$se.fit,col="green", lty=2)
legend(2, 70, legend=c("Predicted with Seasonal Naive Method", "Fit with using real data"),
       col=c("green", "red"), lty=1, cex=0.8)

## Decomposition of temperature data into parts from 1973-2020, additive model
par(mfrow=c(2,2))
climts <- ts(clim$temp, start= c(1973,01,01), end=c(2020,05,18), frequency = 365)
options(scipen=999)
fit_decompose <- decompose(climts , type="additive")
plot(fit_decompose)

## Decomposition of temperature data into parts from 1973-2020, multiplicative model
climts <- ts(clim$temp, start= c(1973,01,01), end=c(2020,05,18), frequency = 365)
options(scipen=999)
fit_decompose <- decompose(climts , type="multiplicative")
plot(fit_decompose)

## Decomposition of temperature data into parts from 2016-2020, additive model
climts <- ts(clim$temp, start= c(2016,03,18), end=c(2020,05,18), frequency = 365)
options(scipen=999)
fit_decompose <- decompose(climts , type="additive")
plot(fit_decompose)

## Decomposition of temperature data into parts from 2016-2020, multiplicative model
climts <- ts(clim$temp, start= c(2016,03,18), end=c(2020,05,18), frequency = 365)
options(scipen=999)
fit_decompose <- decompose(climts , type="multiplicative")
plot(fit_decompose)


## Making up pollution data
t=seq(1,n,1) # This code generates a vector t with values starting from 0 and going up to 10 in the steps of 0.1.
a <- 50 #scaling parameter
y=a*sin(t/365) + runif(length(t),-20,20) # calculates the madeup pollution
df <- data.frame(t,y) 
df<-df[order(df$t),] # orders according to time
polrand <- df$y
clim$pol <- polrand
clim <- clim[order(clim$date),] # orders according to time

# Plot the temperature and pollution on the same graph, ignore what y axis means
plot(clim$date,clim$temp,col="red", ylim = c(-50,95), pch = 16, cex=.4)
points(clim$date,clim$pol,col="blue", pch=16, cex=.4)


## a linear model vs days and temp, seasonality not taken into account.
x<-1:n
y<-clim$temp
a<-lm(y~x)
coeff=coefficients(a)
eq = paste0("y = ", coeff[2], "*x ", coeff[1])
eq
plot(x,y, pch=16,cex=.4,type="l")
abline(a, col="red")
summary(a)


# both acf() and pacf() generates plots by default
acfRes <- acf(clim$temp, lag.max = 365*1) # autocorrelation
pacfRes <- pacf(clim$temp, lag.max = 365*1)  # partial autocorrelation
ccfRes <- ccf(clim$temp, clim$pol, ylab = "cross-correlation", lag.max = 365*1) # computes cross correlation between 2 timeseries.


obje<-forecast(arima1, 14, level = c(80, 95))
obje[4]

length(obje)
autoplot(forecast(arima1, 14, level = c(80, 95)))

forecast(arima1,50, level = c(80, 95))
accuracy(forecast(arima1, 50, level = c(80, 95)))

$#test_timeseries <- ts(clim$temp, frequency = 365, start = c(2020,91))

arima2 <- forecast::Arima(clim$temp, order=c(1,2,1), stationary = FALSE, seasonal = c(), max.D = 365,)
accuracy(arima1)

arima(clim$temp,order = c(4,0,5),include.mean = FALSE, xreg = clim$pol)
