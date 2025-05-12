#Author: Isa Alatovic | ALTISA001

#Load source data:
data("ToothGrowth")
tooth_growth.data <- ToothGrowth

#Increase level of precision:
options(digits = 12)

#We must convert the dose variable to be categorical, as R will interpret it as numerical otherwise:
tooth_growth.data$dose <- as.factor(tooth_growth.data$dose)

#By inspection of the data, supp has 2 levels (VC and OJ) and dose has 3 levels (0.5, 1 and 2)
#In total, there are 2 x 3 = 6 treatments
#There are 60 experimental units
#There are 60/6 = 10 replicates per treatment

#Overall mean of lengths:
len_mean <- mean(tooth_growth.data$len)

#Means of length for each supp level:
tapply(tooth_growth.data$len, tooth_growth.data$supp, mean)

#Means of length for each dose level:
tapply(tooth_growth.data$len, tooth_growth.data$dose, mean)

#Means of length for each treatment (combination of supp and dose):
tapply(tooth_growth.data$len, list(tooth_growth.data$supp, tooth_growth.data$dose), mean)

#Standard deviations of tooth lengths for each treatment:
tapply(tooth_growth.data$len, list(tooth_growth.data$supp, tooth_growth.data$dose), sd)

#We now check the assumptions of two-way ANOVA with interaction (based on the raw data before its fit to a model):
# a) No severe outliers
# b) Homogeneity (equal population variances)
# c) Normality of error terms
# d) Independence of error terms

#Check for outliers by plotting the boxplots for each treatment:
boxplot(tooth_growth.data$len ~ tooth_growth.data$supp * tooth_growth.data$dose, data = tooth_growth.data)

#Check for homogeneity by inspecting the boxplots and comparing IQR (below) and variances/sd (above) for each treatment:
tapply(tooth_growth.data$len, list(tooth_growth.data$supp, tooth_growth.data$dose), IQR)

#Check for normality by creating QQ-plots for each treatment. Without loss of generality, we create the QQ-plot for the VC-0.5 treatment:
qqnorm(tooth_growth.data$len[tooth_growth.data$supp == "VC" & tooth_growth.data$dose == 0.5], pty = 4, main = "VC-0.5 QQ-plot")
qqline(tooth_growth.data$len[tooth_growth.data$supp == "VC" & tooth_growth.data$dose == 0.5], col='red')

#Check for independence of error terms by plotting a dotchart:
dotchart(tooth_growth.data$len)

#We now fit the data to a two-way ANOVA model with interaction:
fit <- aov(len ~ supp * dose, data = tooth_growth.data)

#We then extract model estimates using model.tables():
model.tables(fit, type = "means", se = "TRUE")
model.tables(fit, type = "effects", se = "TRUE")

#We can now print the resulting ANOVA table after fitting the data:
summary(fit)

#We can also perform some more assumption checks now that the data is fitted:

#Check for homogeneity by plotting residuals against fitted values: 
plot(fit, which = 1)

#Check for normality of error terms by plotting a QQ-plot and/or plotting a histogram:
plot(fit, which = 2)
hist(resid(fit))

#Finally, we can create an interaction plot to determine the significance of the interaction between different treatment factors.
#In this case, dose is the x-axis and supplement is the trace variable:
interaction.plot(tooth_growth.data$dose, tooth_growth.data$supp, tooth_growth.data$len)

