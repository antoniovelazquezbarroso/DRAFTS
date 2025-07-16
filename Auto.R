library(tidyverse)
library(ISLR)

# Auto {ISLR} Auto Data Set Gas mileage, horsepower, and other information for 392 vehicles.
?Auto
str(Auto)

# Scatterplot mpg~horsepower
ggplot(Auto, aes(horsepower, mpg))+
  geom_point()

# Regression line with geom_smooth (method="lm")
ggplot(data=Auto, aes(horsepower, mpg)) +   
  geom_point()  +
  geom_smooth(method="lm",se =FALSE)

# Default geom_smooth (method="loess")
ggplot(data=Auto, aes(horsepower, mpg)) +   
  geom_point()  +
  geom_smooth()

# Step by Step
# Run Linear regression
LM <- lm(mpg~horsepower, data=Auto)
summary(LM)
# Get intercept and slope value
coeff<-coefficients(LM)          
intercept<-coeff[1]
slope<- coeff[2]
# Create basic ggplot
ggplot(data=Auto, aes(horsepower, mpg)) +   
  geom_point() +
  # add the regression line
  geom_abline(intercept = intercept,
              slope = slope,
              colour="blue", 
              linetype="dashed", size=1.5
             )+
  # a simpler way to show regression line
  geom_line(aes(x=horsepower, y=predict(LM)), colour="red" )

  # even simpler
ggplot(data=Auto, aes(horsepower, mpg)) +   
  geom_point() +
  geom_line(aes(y=predict(LM)), colour="red" )

# Regression line with abline
plot(Auto$horsepower, Auto$mpg)
abline(LM)

# Regression polynomial 2
LMPoly2 <- lm(mpg ~ horsepower + I(horsepower^2), data=Auto)
summary(LMPoly2)
ggplot(data=Auto, aes(horsepower, mpg)) +   
  geom_point() +
  geom_line(aes(x=horsepower, y=predict(LMPoly2)), colour="blue" )

# Regression polynomial 5
LMPoly5 <- lm(mpg ~ poly (horsepower, 5), data=Auto)
summary(LMPoly5)
ggplot(data=Auto, aes(horsepower, mpg)) +   
  geom_point() +
  geom_line(aes(x=horsepower, y=predict(LMPoly5)), colour="yellow", size=1.5)

# DIBUJANDO TODOS
ggplot(data=Auto, aes(horsepower, mpg)) +   
  geom_point(colour="lightgrey") +
#  geom_smooth() +
  geom_line(aes(y=predict(LM)), colour="red" ) +
  geom_line(aes(y=predict(LMPoly2)), colour="darkgreen" ) +
  geom_line(aes(y=predict(LMPoly5)), colour="yellow" )
