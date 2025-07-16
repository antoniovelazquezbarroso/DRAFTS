# Chapter 6 Lab 1: Subset Selection Methods

# Best Subset Selection

library(ISLR)                             # ISLR incluye el data.frame Hitters
View(Hitters)
names(Hitters)
str(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))
Hitters=na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))

library(leaps)                           # leaps incluye la función regsubsets()
regfit.full=regsubsets(Salary~.,Hitters) # por omision modelos con hasta 8 vbles
summary(regfit.full)    

regfit.full=regsubsets(Salary~.,data=Hitters,nvmax=19) # pedimos hasta 19 vbles
summary(regfit.full)    # muestra el "which", variables incluidas en cada modelo
coef(regfit.full, 1:19) # variables y coeficientes para todos los modelos
coef(regfit.full, 5)    # variables y coeficientes para el modelo con 5 vbles
names(coef(regfit.full, 5)) # solo los nombres de variables para este modelo


reg.summary=summary(regfit.full)
names(reg.summary)  # summary con which (qué vbles), rsq, rss, adjr2, cp y bic
reg.summary$which
reg.summary$rss
reg.summary$rsq
reg.summary$adjr2
reg.summary$cp
reg.summary$bic

par(mfrow=c(2,2))   # plotting rsq, rss, adjr2, cp y bic help choose model
                    
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")

plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
which.max(reg.summary$adjr2)
points(11,reg.summary$adjr2[11], col="red",cex=2,pch=20)

plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
which.min(reg.summary$cp)
points(10,reg.summary$cp[10],col="red",cex=2,pch=20)

plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
which.min(reg.summary$bic)
points(6,reg.summary$bic[6],col="red",cex=2,pch=20)

par(mfrow=c(1,1))

plot(regfit.full,scale="r2")  # builtin plots of selected variables in scale
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")

                     # para el modelo con 6 variables
coef(regfit.full,6)  # las variables elegidas (names) y coef estimados (values)
names(coef(regfit.full,6)) # los nombres de las variables
coef(regfit.full, 6)[1:3]
coef(regfit.full,1:6) # las variables y valor coeficientes para los seis modelos
                      # será útil para hacer cross-validation, porque para hacer
                      # predicciones hay que sacarlos de aquí (regsubset no 
                      # tiene función predict() )

# Forward and Backward Stepwise Selection

regfit.fwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="forward")
summary(regfit.fwd)

regfit.bwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="backward")
summary(regfit.bwd)

coef(regfit.full,7)
coef(regfit.fwd,7)
coef(regfit.bwd,7)

# Choosing Among Models

# Validation set (50%) approach
set.seed(1)                  
train=sample(c(TRUE,FALSE), nrow(Hitters),rep=TRUE) # elige +/- 50%
test=(!train)
regfit.best=regsubsets(Salary~.,data=Hitters[train,],nvmax=19)

summary(regfit.best)
coef(regfit.best, 1:19) # variables y coeficientes para todos los modelos
coef(regfit.best, 5)    # variables y coeficientes para el modelo con 5 vbles
reg.summary=summary(regfit.best)
names(reg.summary)  # summary con which (qué vbles), rsq, rss, adjr2, cp y bic
reg.summary$which
reg.summary$rss
reg.summary$rsq
reg.summary$adjr2
reg.summary$cp
reg.summary$bic

test.mat=model.matrix(Salary~.,data=Hitters[test,]) # Matriz datos validación
val.errors=rep(NA,19)
for(i in 1:19){
   coefi=coef(regfit.best,id=i)
   pred=test.mat[,names(coefi)]%*%coefi             # Aquí calcula la predicción
   val.errors[i]=mean((Hitters$Salary[test]-pred)^2)# y aquí el MSE
}
val.errors
which.min(val.errors)
coef(regfit.best,10)                      # el modelo con 10 vbles era el mejor

                                          # función para calcular predicciones
predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
  }
                                 # ahora lo calcula con todos los datos
regfit.best=regsubsets(Salary~.,data=Hitters,nvmax=19)
coef(regfit.best,10)             # variables elegidas y coeficientes estimados



# Kfold Cross-Validation
k=10                         # k tenfold cross-validation
set.seed(1)
folds=sample(1:k,nrow(Hitters),replace=TRUE)   # elige +/- 10%
cv.errors=matrix(NA,k,19, dimnames=list(NULL, paste(1:19)))
for(j in 1:k){                                  # para cada uno de los k folders
  best.fit=regsubsets(Salary~.,data=Hitters[folds!=j,],nvmax=19)# los 19 modelos
  for(i in 1:19){                  # para cada uno de los modelos con 1:19 vbles
    pred=predict(best.fit,Hitters[folds==j,],id=i)# predicciones test del folder
    cv.errors[j,i]=mean( (Hitters$Salary[folds==j]-pred)^2)# errores test folder
    }
  }
mean.cv.errors=apply(cv.errors,2,mean) # error promedio modelos con 1:19 vbles
mean.cv.errors
which.min(mean.cv.errors)              # el modelo con 11 vbles era el mejor
par(mfrow=c(1,1))
plot(mean.cv.errors,type='b')
                                      # ahora lo calcula con todos los datos
reg.best=regsubsets(Salary~.,data=Hitters, nvmax=19)
coef(reg.best,11).             # variables elegidas y coeficientes estimados


# Chapter 6 Lab 2: Ridge Regression and the Lasso

x=model.matrix(Salary~.,Hitters)[,-1] # de model.matrix quita columna Intercept
y=Hitters$Salary

# Ridge Regression

library(glmnet)
grid=10^seq(10,-2,length=100)             # 100 valores para lambda
                                          # calcula modelo para los 100 lambdas
ridge.mod=glmnet(x,y,alpha=0,lambda=grid) # si alpha=0 ridge, si alpha=1 lasso

dim(coef(ridge.mod)) # 20 filas*coef(Intercept + 19 vbles) , 100 cols*lambdas

ridge.mod$lambda[50]                     # para el modelo 50 valor lambda
coef(ridge.mod)[,50]                     # para el modelo 50 vbles y coefi
sqrt(sum(coef(ridge.mod)[-1,50]^2))      # para el modelo 50 valor L2

ridge.mod$lambda[60]                     # para el modelo 60 valor lambda
coef(ridge.mod)[,60]                     # para el modelo 60 vbles y coefi
sqrt(sum(coef(ridge.mod)[-1,60]^2))      # para el modelo 50 valor L2

           # coeficientes estimados para un modelo con lambda (s) igual a 50
predict(ridge.mod,s=50,type="coefficients")[1:20,]

                                       # para cross-validation 
set.seed(1)                            # split test y training
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]
                                        # estima modelo en train
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid, thresh=1e-12)

ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
mean((ridge.pred-y.test)^2)              # calcula error en test para lambda 4

mean((mean(y[train])-y.test)^2)

ridge.pred=predict(ridge.mod,s=1e10,newx=x[test,])
mean((ridge.pred-y.test)^2) # calcula error en test para lambda muy grande

ridge.pred=predict(ridge.mod,s=0,newx=x[test,],exact=T,x=x[train,],y=y[train])
mean((ridge.pred-y.test)^2) # calcula error en test con lambda 0 (least squares)

lm(y~x, subset=train)
predict(ridge.mod,s=0,exact=T,type="coefficients",x=x[train,],y=y[train])[1:20,]

set.seed(1)                            # cross-validation de glmnet (en train)
cv.out=cv.glmnet(x[train,],y[train],alpha=0) # calcula el MSE para cada lambda
plot(cv.out)
bestlam=cv.out$lambda.min                    # elige el lambda mejor por MSE 
bestlam

ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)           # calcula error en test con este lambda

out=glmnet(x,y,alpha=0) # recalcula modelo con todos los datos con este lambda
predict(out,type="coefficients",s=bestlam)[1:20,]

# The Lasso

lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:20,]
lasso.coef
lasso.coef[lasso.coef!=0]


# Chapter 6 Lab 3: PCR and PLS Regression

# Principal Components Regression

library(pls)
set.seed(2)
pcr.fit=pcr(Salary~., data=Hitters,scale=TRUE,validation="CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type="MSEP")
set.seed(1)
pcr.fit=pcr(Salary~., data=Hitters,subset=train,scale=TRUE, validation="CV")
validationplot(pcr.fit,val.type="MSEP")
pcr.pred=predict(pcr.fit,x[test,],ncomp=7)
mean((pcr.pred-y.test)^2)
pcr.fit=pcr(y~x,scale=TRUE,ncomp=7)
summary(pcr.fit)

# Partial Least Squares

set.seed(1)
pls.fit=plsr(Salary~., data=Hitters,subset=train,scale=TRUE, validation="CV")
summary(pls.fit)
validationplot(pls.fit,val.type="MSEP")
pls.pred=predict(pls.fit,x[test,],ncomp=2)
mean((pls.pred-y.test)^2)
pls.fit=plsr(Salary~., data=Hitters,scale=TRUE,ncomp=2)
summary(pls.fit)
