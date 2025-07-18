library(tidyverse)
library(MASS)     # Includes Boston data.frame
library(ISLR)

?Boston
Boston=tibble(Boston)

str(Boston)
View(Boston)
names(Boston)

lm.fit=lm(medv~lstat,data=Boston)
names(lm.fit)

summary(lm.fit)
names(summary(lm.fit))
summary(lm.fit)$coefficients
summary(lm.fit)$sigma
summary(lm.fit)$r.sq
summary(lm.fit)$adj.r.sq

coef(lm.fit)
confint(lm.fit)
summary(lm.fit)$coefficients

predict(lm.fit, data.frame(lstat=(c(5,10,15))), interval="confidence")
predict(lm.fit, data.frame(lstat=(c(5,10,15))), interval="prediction")

predict(lm.fit, data.frame(lstat=(c(5,10,15))), interval="prediction", level=0.95)
predict(lm.fit, interval="prediction", level=0.95)
predict(lm.fit, interval="prediction", level=0.99)


plot(lm.fit$model$lstat,lm.fit$model$medv)
abline(lm.fit)
abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="red")
plot(lm.fit$model$lstat,lm.fit$model$medv,col="red")
plot(lm.fit$model$lstat,lm.fit$model$medv,pch=20)
plot(lm.fit$model$lstat,lm.fit$model$medv,pch="+")
plot(1:20,1:20,pch=1:20)

par(mfrow=c(2,2))
plot(lm.fit)

par(mfrow=c(1,1))
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))


# Multiple Linear Regression

lm.fit=lm(medv~lstat+age,data=Boston)
summary(lm.fit)

lm.fit=lm(medv~.,data=Boston)
summary(lm.fit)

library(car)
vif(lm.fit)

lm.fit1=lm(medv~.-age,data=Boston)
summary(lm.fit1)

lm.fit1=update(lm.fit, ~.-age)

# Interaction Terms

summary(lm(medv~lstat*age,data=Boston))

# Non-linear Transformations of the Predictors

lm.fit2=lm(medv~lstat+I(lstat^2))
summary(lm.fit2)
lm.fit=lm(medv~lstat)
anova(lm.fit,lm.fit2)
par(mfrow=c(2,2))
plot(lm.fit2)
lm.fit5=lm(medv~poly(lstat,5))
summary(lm.fit5)
summary(lm(medv~log(rm),data=Boston))

# Qualitative Predictors

fix(Carseats)
names(Carseats)
lm.fit=lm(Sales~.+Income:Advertising+Price:Age,data=Carseats)
summary(lm.fit)
attach(Carseats)
contrasts(ShelveLoc)

# Writing Functions

LoadLibraries
LoadLibraries()
LoadLibraries=function(){
  library(ISLR)
  library(MASS)
  print("The libraries have been loaded.")
}
LoadLibraries
LoadLibraries()

