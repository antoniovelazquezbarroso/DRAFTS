library(tidyverse)
library(ISLR)

#Credit {ISLR}	R Documentation   Credit Card Balance Data
Credit <- tibble(read_csv("ALL_CSV_FILES/Credit.csv"))

dim(Credit)
str(Credit)
names(Credit)
summary(Credit)

# Plotly ScatterPlot
library(plotly)
library(GGally)
data <- Credit
p <- ggpairs(data, title="correlogram with ggpairs()")
ggplotly(p)




# Getting values out of the lm object
LGender <- lm(Balance ~ Gender, data=Credit)
MYOUTPUT <- tibble(LGender$model$Balance, fitted(LGender), resid(LGender), check=fitted(LGender)+ resid(LGender))



ggplot(data = Credit, aes(x=Limit, y = Age))+ geom_point()+ geom_smooth()
ggplot(data = Credit, aes(x=Limit, y = Rating))+ geom_point()+ geom_smooth()
