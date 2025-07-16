# Chaper 5 Lab: Cross-Validation and the Bootstrap

# The Validation Set Approach

library(ISLR)

dim(Auto)
(rows_Auto <- dim(Auto)[1])                   # cuenta y devuelve el número de filas
train <- sample(dim(Auto)[1], dim(Auto)[1]/2) # 1 mitad para training ojo num impares
train <- sample(392,196)

# Playing with sample function
sample(392,196)
sample.int(392, 196)
sample.int(1e6, 12, replace = TRUE)
sample(c("Yes","No", "Maybe"), 1e5, replace = TRUE)

x <- 12 # tambien sirve un rango x <- 1:12
        # tambien sirve un vector x <- c(1,2,3,4,5,6,7,8,9,10,11,12)
sample(x)   # a random permutation
sample(x, replace = TRUE)  # bootstrap resampling
sample(x, 20, replace = TRUE)
binomial <- sample(c(0,1), 20, replace = TRUE) # 20 Bernoulli trials (p=0,5)
mean(binomial)
# 1 million Bernoulli trials (p=0,5) to better estimate mean
binomial <- sample(c(0,1), 1e6, replace = TRUE)
mean(binomial)
# 1 million Bernoulli trials (p=0,02)
binomial <- sample(c(0,1), 1e6, replace = TRUE, prob = c(0.98, 0.02))
mean(binomial)

# Validation Set Approach

set.seed(1)
train=sample(392,196)
# Mucho más claro explicitando data=Auto en la regresión
lm.fit=lm(mpg~horsepower, data=Auto, subset=train)
# Pero si haces attach(Auto) no es necesario explicitarlo de nuevo
#attach(Auto)
#lm.fit=lm(mpg~horsepower,subset=train)

     #predict(lm.fit,Auto)[train]- predict(lm.fit) # Estos son iguales
     #predict(lm.fit)              # Estas son las predicciones del grupo train
     #predict(lm.fit,Auto)[-train] # Estas son las predicciones del grupo test


lm.fit=lm(mpg~horsepower, data=Auto, subset=train)
# Este no necesita attach(Auto)
mean((Auto$mpg - predict(lm.fit,Auto))[-train]^2)
attach(Auto)
# Necesita attach(Auto) para calcular mean, lo pongo porque se repite mucho
mean((mpg-predict(lm.fit,Auto))[-train]^2)

lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)


set.seed(2)
train=sample(392,196)
lm.fit=lm(mpg~horsepower,,data=Auto, subset=train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

# Leave-One-Out Cross-Validation

glm.fit=glm(mpg~horsepower,data=Auto)
coef(glm.fit)
lm.fit=lm(mpg~horsepower,data=Auto)
coef(lm.fit)
library(boot) # Se usa para hacer bootstrap y cross-validation (junto con glm())
glm.fit=glm(mpg~horsepower,data=Auto)
cv.err=cv.glm(Auto,glm.fit)
cv.err$delta

cv.error=rep(0,5)
for (i in 1:5){
 glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
 cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
 }
cv.error

# k-Fold Cross-Validation, todo igual salvo K=10

set.seed(17)
cv.error.10=rep(0,10)
for (i in 1:10){
 glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
 cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
 }
cv.error.10

# The Bootstrap

alpha.fn=function(data,index){
 X=data$X[index]
 Y=data$Y[index]
 return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}

alpha.fn(Portfolio,1:100)  # Sobre las 100 obs originales

set.seed(1)         # Sobre una muestra boostrap de tamaño 100
alpha.fn(Portfolio,sample(100,100,replace=T))

boot(Portfolio,alpha.fn,R=1000) # 1000 veces lo repite

# Estimating the Accuracy of a Linear Regression Model

boot.fn=function(data,index)      # Regresion lineal simple
 return(coef(lm(mpg~horsepower,data=data,subset=index)))

boot.fn(Auto,1:392) # Regresion sobre las 392 obs originales

set.seed(1)               # Sobre un par de muestras boostrap
boot.fn(Auto,sample(392,392,replace=T))
boot.fn(Auto,sample(392,392,replace=T))

boot(Auto,boot.fn,1000)       # Sobre 1000 muestras bootstrap

summary(lm(mpg~horsepower,data=Auto))$coef # Sorprende la diferencia

boot.fn=function(data,index)  # Regresion cuadrática
 coefficients(lm(mpg~horsepower+I(horsepower^2),data=data,subset=index))

set.seed(1)
boot(Auto,boot.fn,1000)         # Sobre 1000 muestras bootstrap

summary(lm(mpg~horsepower+I(horsepower^2),data=Auto))$coef # Menos diferencia

