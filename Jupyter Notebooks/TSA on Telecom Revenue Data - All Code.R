## Load necessary libraries
library(astsa)
library(tseries)
library(forecast)
library(stats)
library(TTR)

## Load dataset into dataframe
url <- "C:/Users/tedda/Desktop/Data Science Portfolio/Machine Learning/Supervised Learning/Time Series & Forecasting/TSA on Telecom Revenue Data/Raw Datasets/teleco_time_series.csv"
teleco_rev <- read.csv(url, header = TRUE, row.names = 'Day')

## Convert the dataset to a time series object
ts_rev <- as.ts(teleco_rev)

## Plot the dataset, dataset with trend line, and differenced data plots
par(mfrow = c(3,1))
x <- (1:length(ts_rev))
plot(ts_rev)
plot(ts_rev)
lines(predict(lm(ts_rev ~ x)), col = 'red')
plot(diff(ts_rev))

## Plot the spectral density using both log = "yes" and "no"
par(mfrow = c(2,1))
spectrum(ts_rev, log = "yes")
spectrum(ts_rev, log = "no")

## Decompose our dataset by using a Simple Moving Average to smooth the data. n = 7 for 7 days per week cycle.
par(mfrow = c(1,1))
plot(SMA(ts_rev, n = 7))

## There doesn't appear to be seasonality in our dataset, but test by using decompose()
decompose(ts_rev)
## Results in an error that our ts has no or less than 2 periods/cycles of seasonality. (Lack of Seasonality)

## Evaluate if the dataset is stationary by using the AD-Fuller test
adf.test(ts_rev)

## Split the data into train and test sets. 
## The training set will contain the data minus the last 60 days (2 months)
## The test set will contain all of the data, including the last 60 days.
ts_train <- ts_rev[0:671]
ts_test <- ts_rev

## Export the train and test datasets.
write.csv(ts_train, "C:/Users/tedda/Desktop/Data Science Portfolio/Machine Learning/Supervised Learning/Time Series & Forecasting/TSA on Telecom Revenue Data/Cleansed Datasets/ts_train.csv", row.names = TRUE)
write.csv(ts_test,"C:/Users/tedda/Desktop/Data Science Portfolio/Machine Learning/Supervised Learning/Time Series & Forecasting/TSA on Telecom Revenue Data/Cleansed Datasets/ts_test.csv", row.names = TRUE)

## Generate the ACF and PACF plots (Auto Correlation Function)
acf2(ts_train)

## Use auto.arima from forecast library to find the best model
autoarima <- auto.arima(ts_train)
summary(autoarima)

## View residual plots of best ARIMA model using sarima()
sarima(ts_train, p = 2, d = 1, q = 0)

# Save and Load Model
model_url <- "C:/Users/tedda/Desktop/Data Science Portfolio/Machine Learning/Supervised Learning/Time Series & Forecasting/TSA on Telecom Revenue Data/Exported Models/TSAModel.rds"
saveRDS(autoarima, model_url)
TSA_model <- readRDS(model_url)

## Forecast the next 60 days by using sarima.for() with the best ARIMA model found above
## Plot the forecast's predictions against the actual revenue values
sarima.for(ts_train, n.ahead = 60, p = 2, d = 1, q = 0)
lines(ts_test)

## View the full plot of the 60 day forecast using forecast() from Forecast library
forecast60 <- forecast(TSA_model, h = 60)
plot(forecast60)
lines(ts_test)

## Print forecast intervals at both 80% and 95% prediction intervals for the last 60 days.
forecast60
