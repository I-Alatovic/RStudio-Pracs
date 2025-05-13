#Author: Isa Alatovic | ALTISA001

#Import necessary packages:
library(forecast)
library(fpp3)
library(ggplot2)
library(urca)

#Import source data:
df.data <- forecast::wineind

#We only want the data between Jan 1980 and December 1985, so we use the window function to select this data:
df.data_win <- window(df.data, c(1980, 1), end = c(1985, 12))

#Explore the time series by printing and plotting it: 
print(df.data)
autoplot(df.data) + ggtitle("Monthly sales by Australian wine makers between Jan 1980 and December 1985") + ylab("Monthly sales")
#By inspecting this plot, we see that there is seasonality (with a period of about 1 year), random variation and an increasing linear trend. There is no cyclicality.

#We now need to apply a variance stabilising transformation in order to make the time series stationary (no trend or seasonality):
#We first calculate a value for lambda that will determine which variance stabilising transformation to apply (thankfully, R does this automatically):
lam <- BoxCox.lambda(df.data_win)

#We now apply the transformation to the original time series using this value of lambda and the BoxCox function:
df.stabilised <- BoxCox(df.data_win, lambda = lam)

#Let's plot the data and see if we've removed the variance:
autoplot(df.stabilised)
#Indeed we have! Now we need to remove the trend and seasonality components. If there was no seasonality, we'd difference the series at lag 1, but since there IS seasonality,
#we will difference at lag 12 (since the data are monthly, and the length of one seasonal variation is 1 year, there are 12 'seasons' in one year, so we difference at lag 12):
df.stabilised_diffed <- diff(df.stabilised, lag = 12)

#Let's see how we did:
autoplot(df.stabilised_diffed)
nsdiffs(df.stabilised_diffed)
#The plot looks stationary enough, and nsdiffs (checks if the data needs to be differenced at lag g to remove seasonality) claims that no more differencing is required.

#As a final check of stationarity, let's apply a KPSS test (for a KPSS test, the null hypothesis states that the time series is stationary, while the alternative hypothesis states that it is not):
df.stabilised_diffed %>% ur.kpss() %>% summary()
#By inspecting the output, we see that the test-statistic has a value of 0.2956 < 0.347 (10%) < 0.463 (5%) < 0.574 (2.5%)< 0.739 (1%), so we cannot reject the null hypothesis at even a significance level of 0.1,
#so we fail to reject the null hypothesis and conclude that the time series (df.stabilised_diffed that is) is stationary.

#Although we're fairly confident that df.stabilised_diffed is stationary, we can also produce an ACF plot that shows the autocorrelation of the time series at different lags.
Acf(df.stabilised_diffed)
#By observing this plot, we can see that the autocorrelation appears to be random and statistically insignificant (all bars are within the 95% confidence bounds), so we can conclude that
#there is no significant autocorrelation in the time series, which provides further evidence of its stationary nature.

autoplot(df.data)

#Let's now forecast future values using a simple forecasting method. Amongst the four simple forecasting methods covered in this course (average, naive, seasonal naive and drift),
#I believe the best is seasonal naive. Although it does not take trend into account, our time series has a strong seasonal component and should be fine for short term predictions.
#NOTE: A better forecasting model would be Holt-Winters, but this is likely not the intended model for this practical as it takes place before Holt-Winters was introduced in class.

#First we set the windowed (training) data to a seasonal naive forecasting model with a forecast horizon of 12, as stated in the prac doc:
df.mod_1 <- snaive(df.data_win, h = 12)

#Then we plot this model alongside our original time series:
autoplot(df.data_win) + autolayer(df.mod_1, PI = F)
#By inspecting this plot of the forecast model, we see that it seems to get all the seasonal peaks and troughs in the right places,
#which makes sense because of the seasonality of this time series, but the forecasted values seem to underestimate the true values
#at most points, which can be attributed to the increasing linear trend of this series.

#Lastly, let's examine the residuals of forecast model to determine its suitability to forecast future values
checkresiduals(df.mod_1)
#The correlogram (and Ljung-Box test) indicates that there is no significant autocorrelation (which means the model has captured all relevant information in the data),
#but we also see that the residuals appear to be approximately normally distributed. The plot of the residuals also seems to be random, with a relatively constant variance as well.
#HOWEVER, the mean of the residuals is NOT zero, which means the forecasts will be biased. We saw this when we plotted the forecast model against the test data.
#Therefore, this model is ultimately unsuitable to forecast future values, especially for long term forecasts, due to the linear trend that is not being captured.
 
