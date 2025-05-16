#Author: Isa Alatovic | ALTISA001

#Load necessary packages:
library(forecast)
library(fpp2)
library(ggplot2)

#Load source data:
df.data <- window(forecast :: wineind, c(1980, 1), c(1987, 12))

#Explore data by printing to console and plotting:
print(df.data)
autoplot(df.data) + ggtitle("Monthly wine sales by Australian winemakers from Jan 1980 to Dec 1987") + ylab("Monthly wine sales")

#In this time series, we see a seasonal component with period of 1 year, increasing linear trend, random variation and no cyclicality.

#Partition data into training data and test data:
df.training <- window(df.data, c(1980, 1), c(1986, 12))
df.test <- window(df.data, c(1986, 1))

#We will now fit two exponential smoothing models; one to capture the trend, the other to capture the trend and seasonality.
#To capture the trend, we will use Holt's Linear Trend method, and to capture trend and seasonality we will use Holt-Winters.
#We will specify a forecast horizon of 12 time periods:
df.mod1 <- holt(df.training, h = 12)
df.mod2 <- hw(df.training, h = 12, seasonal = 'multiplicative')

#We now print out a summary for each model:
df.mod1[["model"]]
df.mod2[["model"]]

#And we plot these forecast models alongside the original time series:
autoplot(df.data) + autolayer(df.mod1) + autolayer(df.mod2)

#And again without prediction intervals:
autoplot(df.data) + autolayer(df.mod1, PI = F) + autolayer(df.mod2, PI = F)

#Let's now examine the model fit for each of these models:
checkresiduals(df.mod1)
checkresiduals(df.mod2)
#There is strong autocorrelation in df.mod1. This is evident by the extremely small p-value for the Ljung-Box test, and the clear seasonal pattern in the residuals.
#Further, we notice a strong autocorrelation at lag 12, and a moderate autocorrelation at lag 24. The residuals do however appear to be normally distributed with mean
#of zero and the variance appears constant. That said, the strong autocorrelation means that this model is not a good fit for the data.

#In terms of goodness-of-fit, df.mod2 performs much better, but still has some autocorrelation. The p-value is such that we can conclude that there is significant autocorrelation at at least one lag up to lag 17,
#namely at lag 4. The variance does appear somewhat constant, and the residuals appear approximately normally distributed with a mean close to 0. Overall, this model is a better
#fit than df.mod1, but is still not perfect.

#Finally, observe that the AIC and AICc for df.mod2 are both less than that of df.mod1, indicating a better fit.

#Overall, I'm happy to proceed with df.mod2, but I wouldn't proceed with df.mod1.

#Now, lets check each model's accuracy for forecasting:
accuracy(df.mod1, df.test)
accuracy(df.mod2, df.test)

#Observe the RMSE and MAE values for df.mod1 and df.mod2. Clearly both of these values for df.mod2 are less than the corresponding values for df.mod1.
#This indicates that df.mod2 provides a more accurate forecast for future values compared to df.mod1. This makes sense because df.mod2 takes into account
#trend AND seasonality, while df.mod1 only captures trend.
