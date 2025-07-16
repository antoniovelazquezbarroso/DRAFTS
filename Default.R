library(tidyverse)
library(ISLR)


?Default
# Default {ISLR}	R Documentation   Credit Card Default Data
# ¿Transformar el dataframe en tibble? Creo innecesario. Elimina factors
# Default <- tibble(read_csv("ALL_CSV_FILES/Default.csv"))

dim(Default)
str(Default)
names(Default)
summary(Default) # Si dataframe, muestra los niveles y recuentos en factors

Default |> group_by(default) |> summarise(n())
Default |> count(default) # Más corto

Default |> group_by(student) |> summarise(n())


ggplot(data=Default, aes(balance, income, colour=default, shape=default)) + geom_point()
ggplot(data=Default, aes(default, balance)) + geom_boxplot()
ggplot(data=Default, aes(default, income)) + geom_boxplot()


# See https://patchwork.data-imaginist.com/
library(patchwork)
plot_points <- ggplot(data=Default, aes(balance, income, colour=default, shape=default)) +
                 geom_point()
plot_balance <- ggplot(data=Default, aes(default, balance)) +
                 geom_boxplot()
plot_income <- ggplot(data=Default, aes(default, income)) + 
                 geom_boxplot()

plot_income + plot_balance
plot_income | plot_balance
plot_points | plot_income | plot_balance
plot_points/(plot_income | plot_balance)


# 
