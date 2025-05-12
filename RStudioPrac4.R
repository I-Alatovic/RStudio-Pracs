#Author: Isa Alatovic | ALTISA001

#Import source data:
drying.wide <- read.csv("drying_times_wide.csv")
drying.long <- read.csv("drying_times_long.csv")

#View data:
View(drying.wide)
View(drying.long)

#Calculate means using wide data:
paint1.mean <- mean(drying.wide$Paint.1)
paint2.mean <- mean(drying.wide$Paint.2)
paint3.mean <- mean(drying.wide$Paint.3)
paint4.mean <- mean(drying.wide$Paint.4)

#Alternatively, you can also calculate the means using long data:
#e.g paint1.mean <- mean(drying.long$drying_time[drying.long$paint == "Paint.1"])

#Calculate standard deviations:
paint1.sd <- sd(drying.wide$Paint.1)
paint2.sd <- sd(drying.wide$Paint.2)
paint3.sd <- sd(drying.wide$Paint.3)
paint4.sd <- sd(drying.wide$Paint.4)

#Alternatively, you can also calculate the standard deviations using the long data:
#e.g paint1.sd <- sd(drying.long$drying_time[drying.long$paint == "Paint.1"])

#Explore the wide dataset using head() and summary():
head(drying.wide)
summary(drying.wide)

#We will now use the long dataset. The treatment factor is already categorical,
#but if it was numerical, we would convert it as follows:
#drying_long$paint <- as.factor(drying_long$paint)

#We now check the assumptions for one-way ANOVA:
# a) No severe outliers
# b) Homogeneity (equal population variances)
# c) Normality of error terms
# d) Independence of error terms

#Check for outliers using a boxplot with points overlayed (optional):
drying.boxplot <- boxplot(drying_time ~ paint, data = drying.long)
stripchart(drying_time ~ paint, data = drying.long, vertical = TRUE, add = TRUE, method = "jitter")

#Check for homogeneity by comparing IQRs and SDs (also check boxplots):
sort(tapply(drying.long$drying_time, drying.long$paint, sd))
sort(tapply(drying.long$drying_time, drying.long$paint, IQR))

#Check for normality of error terms by plotting a QQ-plot of the data and/or a histogram of residuals
#(Once again, you can also check boxplots!). First we extract the data for each treatment:
paint1 <- drying.long$drying_time[drying.long$paint=="Paint.1"]
paint2 <- drying.long$drying_time[drying.long$paint=="Paint.2"]
paint3 <- drying.long$drying_time[drying.long$paint=="Paint.3"]
paint4 <- drying.long$drying_time[drying.long$paint=="Paint.4"]

#Now we plot create the QQ-plots for each treatment:
paint1QQ <- qqnorm(paint1, pty = 4, col = "green", main = "Paint 1")
qqline(paint1, col='red')
paint2QQ <- qqnorm(paint2, pty = 4, col = "green", main ="Paint 2")
qqline(paint2, col='red')
paint3QQ <- qqnorm(paint3, pty = 4, col = "green", main ="Paint 3")
qqline(paint3, col='red')
paint4QQ <- qqnorm(paint4, pty = 4, col = "green", main ="Paint 4")
qqline(paint4, col='red')

#Furthermore, we can also plot a histogram of residuals. First we fit the data to a one-way ANOVA model:
drying.model <- aov(drying_time ~ paint, data = drying.long)

#Then we plot the histogram (in this case, the sample size is quite small so the histogram
#probably isn't the best way to check for normality): 
hist(residuals(drying.model), main = "Histogram of residuals", xlab = "Residuals")

#Lastly, we check for independence using a dotchart, with the y-axis representing the order of observation:
dotchart(drying.long$drying_time)

#Now we can extract the model estimates for the treatment means and effects:
model.tables(drying.model, type = "means", se = TRUE)
model.tables(drying.model, type = "effects", se = TRUE)








