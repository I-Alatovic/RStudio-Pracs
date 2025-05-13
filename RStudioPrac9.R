#Author: Isa Alatovic | ALTISA001

#Install necessary packages:
library(tidyverse)
library(ggplot2)
library(fpp2)
library(forecast)

#Import source data and convert to time series using the as.ts function:
df.data <- as.ts(forecast :: wineind)

#Explore the data by printing it and plotting using autoplot:
print(df.data)
df.plot <- autoplot(df.data) + ggtitle("Monthly sales by Australian wine makers from Jan 1980 to Aug 1994") + ylab("Monthly sales")

#By inspecting the plot, we can see seasonality, random variation and an increasing non-linear trend. The period of each season is about 1 year.

#Since this data is monthly, the appropriate value of k would be 12.

#To find the CMA(12) for July 1985, there are two method: by hand or using R:

#By hand:
print(ma(df.data, 12))
#By inspecting this output, the CMA(12) value for July 1985 is 27093.79

#Using R (R will automatically select the appropriate k value, and t = 67 corresponds to July 1985 in this data):
df.decomp <- decompose(df.data, type = 'multiplicative')
df.decomp$trend[67]
#By running this, you'll see that the output is 27093.79, the same as before.

#We will now add the CMA(12) plot to our current plot of the time series using the autolayer function:
df.plot <- df.plot + autolayer(ma(df.data, 12), series = "MA12")
#By inspection, this smoothed series does indeed remove the seasonality.

#Print out and explore the classical decomposition model:
print(df.decomp)
plot(df.decomp)
#By inspecting the plot of the decomposition model, we can conclude that there is no cyclicality in the time series.
#The plot of the trend component does not show any cyclicality, and the random components does not appear to have any pattern.