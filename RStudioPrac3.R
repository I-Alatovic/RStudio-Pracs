#Author: Isa Alatovic | ALTISA001

#Import source data:
depression.data <- read.csv("logreg.csv")
hospital_stay.data <- read.csv("step.csv")

#Fit depression data to a binomial linear model (to perform logistic regression):
fit_depression <- glm(cases ~ sex + income, data = depression.data, family = 'binomial')

#Print summary of logistic regression:
summary(fit_depression)

#Convert numeric variables medschl and region in hospital_stay to categorical by using as.factor():
hospital_stay.data$medschl <- as.factor(hospital_stay.data$medschl)
hospital_stay.data$region <- as.factor(hospital_stay.data$region)

#Fit hospital_stay.data to a linear model using all available explanatory variables (full model):
fit_hospital.full <- lm(length ~ ., data = hospital_stay.data)

#Print summary of multiple linear regression of full model:
summary(fit_hospital.full)

#Now fit hospital_stay.data to a linear model with only an intercept (empty model):
fit_hospital.empty <- lm(length ~ 1, data = hospital_stay.data)

#Perform forward selection process using step():
step.model <- step(fit_hospital.empty, direction = 'forward', scope = list(lower = fit_hospital.empty, upper = fit_hospital.full))

#Print summary of multiple linear regression of step.model:
summary(step.model)