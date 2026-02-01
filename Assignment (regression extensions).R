#Chrissa Muller
# ---------------------------------------------
# Assignment (regression extensions)
# ---------------------------------------------

#Load Libarary
library(ggplot2)

#Data
wages <- wages
str(wages)

#Plot Wage Against Age
ggplot(wages, aes(x = Age, y = Wage)) + geom_point() +
  labs(title = "Hourly Wage vs Age", x = "Age", y = "Hourly Wage")

#Multiple Regression Model of Wage(Age and Education as independent (x) variables)
model_multiple_regression <- lm(Wage ~ Age + Educ, data = wages)
summary(model_multiple_regression)
hist(residuals(model_multiple_regression), main = "Residuals -Linear Model", xlab = "Residuals")

#Multiple Regression Model Using Quadratic Relationship for Age
model_quadratic_regression <- lm(Wage ~ Age + I(Age^2) + Educ, data = wages)
summary(model_quadratic_regression)
hist(residuals(model_quadratic_regression), main = "Residuals - Quadratic Model", xlab = "Residuals")

#Comparing Goodness of Fit
summary(model_multiple_regression)$r.squared
summary(model_quadratic_regression)$r.squared

#Appropriate Model to Predict Hourly Wages for Someone with 16 Years of Education and Age Equal 30, 50, or 70
Workers <- data.frame(Age = c(30, 50, 70), Educ = 16)
predict(model_quadratic_regression, newdata = Workers)

#Age That Someone with 16 Years of Education will Attain the Highest Wages
# Extract coefficients
coefficients <- coef(model_quadratic_regression)

#Turning point formula
Age <- -coefficients["Age"] / (2 * coefficients["I(Age^2)"])
Age

#Data
AnnArbor <- AnnArbor
str(AnnArbor)

#Plot Rent Against Three Predictor Variables
ggplot(AnnArbor, aes(x = Sqft, y = Rent)) + geom_point()
ggplot(AnnArbor, aes(x = Beds, y = Rent)) + geom_point()
ggplot(AnnArbor, aes(x = Baths, y = Rent)) + geom_point()

#Log-Transformations Potentials
AnnArbor$log_rent <- log(AnnArbor$Rent)
AnnArbor$log_sqft <- log(AnnArbor$Sqft)

# Regression model
model_rent <- lm(log_rent ~ log_sqft + Beds + Baths, data = AnnArbor)
summary(model_rent)

#Multiple Regression Model to Predict Rent for 1,600 Square-Foot Rental with 3 Bedrooms and 2 Bathrooms
rental <- data.frame(log_sqft = log(1600), Beds = 3, Baths = 2)
rent_prediction <- predict(model_rent, rental)
rent_prediciton_dollars <- exp(rent_prediction)
rent_prediciton_dollars
