#Author: Isa Alatovic | ALTISA001

#Import necessary package:
library(readxl)

#Increase precision:
options(digits = 12)

#Import source data:
goals.data <- read_xlsx('Goals_data.xlsx')
cars.data <- read_xlsx('Cars_data.xlsx')

#Experiment 1:
#This experiment has 5 blocked samples (the 5 attackers) and 3 treatments (the kits: home, away and alternate),
#so we perform the Friedman Test:

#We first explore the data using boxplots to determine normality:
boxplot(goals.data$Goals ~ goals.data$Player)

#Then we perform the test using the friedman.test function:
friedman.test(y = goals.data$Goals, groups = goals.data$Player, blocks = goals.data$Kit)

#P-value = 0.45, so we reject the null hypothesis at 1% (and every other standard signicance level) and conclude that
#at least one attacker has a median number of goals different from the others.

#Experiment 2:
#This experiment involves determining whether there is a positive correlation between two variables.
#Normally we'd conduct Pearson's Correlation Coefficient Test to determine if they are correlated,
#but if either variable is not normally distributed we cannot do this.

#Let's first check for normality using boxplots and histograms:
boxplot(cars.data$Max_speed)
boxplot(cars.data$Noise_level)
hist(cars.data$Max_speed)
hist(cars.data$Noise_level)
#Both boxplots seem to suggest that the distributions of the variables COULD be non-normal,
#but this is confirmed when inspecting the histograms.

#And we can also check whether an association is likely using a scatterplot:
plot(cars.data$Noise_level, cars.data$Max_speed, main = "Scatterplot of Noise Level vs Max Speed", xlab = "Noise Level", ylab = "Max Speed", pch = 19)

#Since the data are non-normal, we cannot use Pearson's Correlation Coefficient Test, so we must use a non-parametric test.
#The corresponding non-parametric test is the Spearman Correlation Test, as performed here:
cor.test(cars.data$Noise_level, cars.data$Max_speed, method = "spearman", alternative = "greater", exact = FALSE)

#Here, since the alternative hypothesis is that Noise Level and Maximum Speed are positively correlated, the "alternative" parameter is set to "greater"
#Note that this follows the same format Pearson's Correlation Coefficient Test, except the "method" parameter is different.

#Once again, the manual methods are omitted as they are very tedious, but they are described in detail in the notes and videos. Make sure you know how to do them!
