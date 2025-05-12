#Author: Isa Alatovic | ALTISA001

----------------------------------
#NOTE: This section is already well covered in the lecture videos for NP tests on Amathuba.
#I highly recommend you watch those first
----------------------------------
  
#Increase precision:
options(digits = 12)

#Import necessary package:
library(readxl)

#Import source data:
flights.data <- read_xlsx("flight_times.xlsx")
happy.data <- read_xlsx("happy_vids.xlsx")
plants.data <- read.csv("plant_growth.csv")

#First let's determine which non-parametric tests are appropriate for each experiment:
#flights.data has TWO samples and the data is PAIRED/dependent, so the appropriate test is the Wilcoxon Signed Rank Test
#happy.data has TWO samples and the data is INDEPENDENT, so the appropriate test is the Wilcoxon-Mann-Whitney Test/U-Test
#plants.data has THREE samples and the data is INDEPENDENT, so the appropriate test is the Kruskal-Wallis Test

#Before we proceed, we need to understand why we're using non-parametric tests. We typically use non-parametric techniques when one or more of the following conditions are met:
# a) Data is not normal
# b) Small sample size
# c) Data is ordinal or ranked
# d) Severe outlier(s) present
# e) Assumption of equal sample variances (homoscedasticity) is violated

#For example, let's inspect flights.data. The data is paired, so we'll be performing a test on the differences between the samples.
#Let's look at the histogram for these differences. If the shape is not normal, we should not use a parametric test:
flights.diff <- flights.data$Over_capacity_flight - flights.data$Under_capacity_flight
hist(flights.diff, main = "Histogram of paired differences between over-capacity flights and under-capacity flights", xlab = "over-capacity - under-capacity")
#This histogram is clearly negatively skewed and hence non-normal, so we can stop here, but we can also look at a boxplot and a QQ-plot:
boxplot(flights.diff, main = "Boxplot of paired differences", ylab = "over-capacity - under-capacity")
stripchart(flights.diff, vertical = TRUE, add = TRUE, method = "jitter")
qqnorm(flights.diff, main = "QQ-plot of paired differences")
qqline(flights.diff, col = "red")
#As you can see, the QQ-plot seems to indicate normality, but by inspecting the boxplot, there is clearly
#one extreme outlier that disqualifies this experiment from parametric tests
#A similar procedure can be carried out to check why we're using non-parametric tests for the other experiments

#With that out of the way, let's begin with flights.data:
wilcox.test(flights.data$Under_capacity_flight, flights.data$Over_capacity_flight,
                           paired = TRUE, alternative = "less", exact = FALSE)

#And happy.data:
wilcox.test(happy.data$Darwin, happy.data$Kittens_and_Puppies, paired = FALSE, alternative = "greater", exact = FALSE)

#And lastly, plants.data:
kruskal.test(Tomatoes ~ Fertilizer, data = plants.data)

#We omit the manual version of these tests due to time constraints, but there are many examples on Amathuba - in the slides and RStudio videos. 