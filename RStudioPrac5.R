#Author: Isa Alatovic | ALTISA001

#Import source data:
oat_variety.data <- read.csv("oat_variety.csv")

#Extract data for each treatment
gold.data <- oat_variety.data$yield[oat_variety.data$variety == "Golden.rain"]
victory.data <- oat_variety.data$yield[oat_variety.data$variety == "Victory"]
marvellous.data <- oat_variety.data$yield[oat_variety.data$variety == "Marvellous"]

#Calculate the mean for each treatment:
sort(tapply(oat_variety.data$yield, oat_variety.data$variety, mean))

#Calculate the variance for each treatment:
sort(tapply(oat_variety.data$yield, oat_variety.data$variety, var))

#Calculate the SD for each treatment:
sort(tapply(oat_variety.data$yield, oat_variety.data$variety, sd))

#We must now check the assumptions of the two-way ANOVA model without interaction.
#We will focus on methods that can be performed before the model is fitted
# a) No severe outliers
# b) Homogeneity (equal population variances)
# c) Normality of error terms
# d) Independence of error terms
# e) Additivity (no interaction between blocks and treatments)

#Check for severe outliers using a boxplot:
boxplot(yield ~ variety, data = oat_variety.data)

#Check for homogeneity. Look at the boxplots again, and compare IQRs and variances (calculated above):
sort(tapply(oat_variety.data$yield, oat_variety.data$variety, IQR))

#Check for normality of error terms using a QQ-plot for each treatment:
gold.qq <- qqnorm(gold.data, pty = 4, col = "green", main = "Gold.rain QQ-plot")
qqline(gold.data, col='red')
victory.qq <- qqnorm(victory.data, pty = 4, col = "green", main = "Victory QQ-plot")
qqline(victory.data, col='red')
marvellous.qq <- qqnorm(marvellous.data, pty = 4, col = "green", main = "Marvellous QQ-plot")
qqline(marvellous.data, col='red')

#Check for independence of error terms by plotting yield against order of observation (look for random spread):
dotchart(oat_variety.data$yield)

#Lastly, check for additivity (additivity assumption is met if the majority of the lines are parallel):
interaction.plot(oat_variety.data$plot, oat_variety.data$variety, oat_variety.data$yield)

#We now fit the data to a two-way ANOVA model:
rcbd_model <- aov(yield ~ variety + plot, data = oat_variety.data)

#We can then extract model estimates using model.tables():
model.tables(rcbd_model, type = 'means', se = 'TRUE')
model.tables(rcbd_model, type = 'effects', se = 'TRUE')

#Finally, we can print the ANOVA table
summary(rcbd_model) #or anova(rcbd_model)

#Bonus: How to calculate the 95% confidence interval for a treatment mean
#Suppose we wanted to calculate a 95% confidence interval for the mean yield of Marvellous oats:
t_multiplier <- qt(0.025, 10, lower.tail = F) #t-distribution critical value with residual df
se <- 5.510 #Standard error of effects for variety from earlier (sqrt(182.2/6), where 182.2 = MSE of residuals and 6 = number of replicates)
mean_marvellous <- mean(marvellous.data) #mean of yield for marvellous oats
upper <- mean_marvellous + t_multiplier*se #upper bound
lower <- mean_marvellous - t_multiplier*se #lower bound
