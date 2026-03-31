install.packages("Hmisc")
library(Hmisc)

class(data_r)
class(data_r_test)

# Checking for missing values in both series

colSums(is.na(data_r))
colSums(is.na(data_r_test))

# Converting 'FFD_series' to time series and storing in 'climate'

climate <- ts(data_r[, 2:5], 
              start = c(2013, 1), 
              frequency = 365)

temperature <- ts(data_r$meantemp, 
                  start = c(2013, 1), 
                  frequency = 365)

humidity <- ts(data_r$humidity, 
               start = c(2013, 1), 
               frequency = 365)

wind_speed <- ts(data_r$wind_speed, 
                 start = c(2013, 1), 
                 frequency = 365)

pressure <- ts(data_r$meanpressure, 
               start = c(2013, 1), 
               frequency = 365)

# Checking conversion has been successfully done for each series

class(climate)
class(data_r)
class(pressure)
class(wind_speed)
class(humidity)
class(temperature)


plot(climate, ylab='climate_data', xlab='Time period', type='o', 
     col='blue', main = 'Figure 1: Time-series plot of change in daily values')

# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

# Calculating correlation

res1<-rcorr(as.matrix(climate))
flattenCorrMatrix(res1$r, res1$P)

# Plotting ACF and PACF 
par(mfrow=c(1,2)) 
acf(temperature, lag.max = 48, main = "Figure 2.1: Temperature ACF ")
pacf(temperature, lag.max = 48, main = "Figure 2.2: Temperature PACF")
# Temperature is non-stationary

par(mfrow=c(1,2)) 
acf(humidity, lag.max = 48, main = "Figure 2.3: Humidity ACF ")
pacf(humidity, lag.max = 48, main = "Figure 2.4: Humidity PACF")
# Humidity is non-stationary

par(mfrow=c(1,2))
acf(wind_speed, lag.max = 48, main = "Figure 2.5: Wind Speed ACF ")
pacf(wind_speed, lag.max = 48, main = "Figure 2.6: Wind Speed PACF")
#Wind speed is stationary

acf(pressure, lag.max = 48, main = "Figure 2.7: Pressure ACF ")
pacf(pressure, lag.max = 48, main = "Figure 2.8: Pressure PACF")
#Pressure is fully stationary
#-------------------------------------------------------------------------------

install.packages("tseries")
library(tseries)
#Dicker-Fuller Unit-Root test (ADF)

adf.test(temperature, k=ar(temperature)$order)
#The ADF test of 11 lag order for FFD indicates series is non-stationary as p-value is greater than 5% level of significance.

adf.test(humidity, k=ar(humidity)$order)
#The ADF test rejects the null hypothesis of a unit root at the 5% level (p = 0.036), indicating that humidity is stationary.

adf.test(wind_speed, k=ar(wind_speed)$order)
#The ADF test of 0 lag order for wind speed indicates series is stationary as p-value is less than 5% level of significance supporting the ACF & PACF plots plotted for it.

adf.test(pressure, k=ar(pressure)$order)
#The ADF test of 0 lag order for wind speed indicates series is stationary as p-value is less than 5% level of significance supporting the ACF & PACF plots plotted for it.

#Transformation of Temperature (non-stationary)
temperature_diff = diff(temperature)
plot(temperature_diff,ylab='Temperature',xlab='Time period', col="blue", 
     main = "Figure 3: Time series plot of first differenced Temperature")

#Performing ADF test to check stationarity of first order differenced Temperature:
adf.test(temperature_diff)

#Decomposition
#ModelinG using Distributed Lag Model (DLM)


# Applying finiteDLMauto to calculate best model based on AIC, BIC and MASE values
install.packages("dLagM")   
library(dLagM)

finiteDLMauto( x=as.vector(humidity), y= as.vector(temperature), q.min = 1, q.max = 10, 
               model.type="dlm", error.type = "AIC",trace=TRUE)

model.humidity = dlm(x=as.vector(humidity), y=as.vector(temperature), q=10)
model.wind_speed = dlm(x=as.vector(wind_speed), y=as.vector(temperature), q=10)
model.pressure = dlm(x=as.vector(pressure), y=as.vector(temperature), q=10)

AIC(model.humidity$model,model.wind_speed$model,
               model.pressure$model)

BIC(model.humidity$model,model.wind_speed$model,
    model.pressure$model)

MASE(model.humidity$model,model.wind_speed$model,
     model.pressure$model)

model.finite2 = dlm(x=as.vector(humidity), y=as.vector(temperature), q=10)
summary(model.finite2)

install.packages("car")
library(car)
vif(model.finite2$model)

#Residual analysis of model.finite2 model
install.packages("forecast")   # run once
library(forecast)
checkresiduals(model.finite2$model,test=FALSE)

#Forecasting using model.finite2
x <- data_r_test[,3]
x

# Generating prediction intervals & point forecasts
fit <- auto.arima(humidity)
forecast(fit, h = 4)
# Generating point forecasts

fc <- forecast(fit, h = 4)

fc$mean   # point forecast
fc$lower  # lower interval
fc$upper  # upper interval

fc <- forecast(fit, h = 365*4)

plot(fc,
     main = "Temperature Forecast (Next 4 Years)",
     xlab = "Year",
     ylab = "Temperature",
     col = "blue",
     flty = 2)   # dashed forecast line

lines(temperature, col = "black")  # actual data
