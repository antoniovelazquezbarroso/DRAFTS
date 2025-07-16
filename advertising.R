library(tidyverse)

Adv <- tibble(read_csv("ALL_CSV_FILES/Advertising.csv"))
Adv <- Adv %>% rename(market= ...1) %>% 
               mutate(budget=TV+radio+newspaper)

str(Adv)
names(Adv)

# Sales~budget
LB <- lm(sales~budget, data=Adv)
summary(LB) # See F-statistic


# Sales~TV+radio+newspaper
LM <- lm(sales ~ TV+radio+newspaper, data=Adv)
summary(LM)
cor(Adv[2:5])

# Sales~TV
LTV <- lm(sales~TV, data=Adv)
summary(LTV)

# Draw scatterplot and regression line
# Get intercept and slope value
coeff<-coefficients(LTV)          
intercept<-coeff[1]
slope<- coeff[2]

ggplot(Adv,aes(TV, sales))+ 
  geom_point()+
  geom_abline(intercept = intercept,
              slope = slope, color="red", 
              linetype="dashed", size=1.5)

ggplot(Adv,aes(TV, sales))+ 
  geom_point()+
  geom_smooth(method="lm")

