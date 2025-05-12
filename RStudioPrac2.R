#Author: Isa Alatovic | ALTISA001

#Import source data:
properties.data <- read.csv("multreg.csv")

#Generate correlation matrix (all rows, columns 1 to 5):
cor(properties.data[,1:5])

#Fit data to linear model so that we can perform multiple linear regression:
#Note: additional explanatory variables are added with +, while interaction
#terms between terms are denoted by a *
fit <- lm(Price ~ PlotSize + FloorArea + Trees + Distance + PlotSize*FloorArea + Pool, data = properties.data)

#Print summary of linear regression and 95% confidence intervals:
summary(fit)
confint(fit)
