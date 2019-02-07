wine <- read.csv("wine.csv")
names(wine)
str(wine)
summary(wine)
# Question 1A.) plot Price vs independent variables:
plot(Price~., data = wine)
# Question 1B.) Create Regression Model:
fit <- lm(Price~ + Year + Temp + Rain + PRain + Age, data = wine)
fit <- lm(Price~ + Temp + Rain + PRain + Age, data = wine)
summary(fit)

# Diagnostic Checks:
plot(fit$res~fit$fitted, main = "Residuals vs Fitted")
hist(fit$res, col = c("Blue", "red", "green"))
qqnorm(fit$res)
shapiro.test((fit$res))

# BoxCox
library(MASS)
boxcox(Price~ + Temp + Rain + PRain + Age, data = wine)
# Take the log of price:
wine$logprice <- log(wine$Price)

# Refit Model:
fit <- lm(wine$logprice~ + Temp + Rain + PRain + Age, data = wine)
summary(fit)
plot(fit$res~fit$fitted, main = "Residuals vs Plot")
hist(fit$res, col = c("green", "blue", "red"))
qqnorm(fit$res)

# Plot Final Model:
plot(wine$logprice~., data = wine)
fit <- lm(wine$logprice~ + Temp + Rain + Age, data = wine)
summary(fit)
plot(fit$res~fit$fitted, main = "Residuals vs Plot")
hist(fit$res, col = c("green", "blue", "red"))
qqnorm(fit$res)
shapiro.test(fit$res)
