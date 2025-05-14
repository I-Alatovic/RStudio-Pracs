#Author: Isa Alatovic | ALTISA001

#Import necessary package:
library(readxl)

#Import source data:
calls.data <- read_excel("calls.xlsx")

#Extract individual variables:
executions.data <- calls.data$Executions
numcalls.data <- calls.data$Calls

#Calculate means:
executions.mean <- mean(executions.data)
numcalls.mean <- mean(numcalls.data)

#Calculate variances:
executions.var <- var(executions.data)
numcalls.var <- var(numcalls.data)

#Calculate standard deviations:
executions.sd <- sd(executions.data)
numcalls.sd <- sd(numcalls.data)

#Calculate correlation coefficient:
corr_calls_exe <- cor(executions.data,numcalls.data)

#Plot data with number of calls on the x-axis, number of trade executions
#on the y-axis, pch = 20 (this means that the marker is a bullet point)
#and appropriate axis labels:
plot(x = numcalls.data, y = executions.data, pch = 20, xlab = "Number of incoming calls per day",
     ylab = "Number of trade executions per day")

#Fit data to a linear model so that we can perform linear regression:
fit <- lm(executions.data ~ numcalls.data, data = calls.data)

#Print out linear regression summary, anova table and 95% confidence intervals:
summary(fit)
anova(fit)
confint(fit)
