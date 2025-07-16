library(tidyverse)

# mpg {ggplot2} Fuel economy data from 1999 to 2008 for 38 models of cars
?mpg 

# Scatterplot with regression line

# Just in one step with geom_smooth
ggplot(mpg)+
  geom_point(aes(hwy, cty, colour=class))+
  geom_smooth(aes(hwy, cty), method="lm",se =FALSE)

# Step by Step
# Run Linear regression
cor(mpg$cty, mpg$hwy)
LM <- lm(cty~hwy, data=mpg)
summary(LM)
# Get intercept and slope value
coeff<-coefficients(LM)          
intercept<-coeff[1]
slope<- coeff[2]
# Create basic ggplot
ggplot(data=mpg, aes(hwy, cty)) +   
         geom_point() +
# add the regression line
         geom_abline(intercept = intercept, slope = slope, color="red", 
                     linetype="dashed", size=1.5)

# All in the same plot
ggplot(mpg)+
  geom_point(aes(hwy, cty, colour=class))+
  geom_smooth(aes(hwy, cty), method="lm")+
  geom_abline(intercept = intercept, slope = slope, color="yellow", 
              linetype="dashed", size=1.5)

