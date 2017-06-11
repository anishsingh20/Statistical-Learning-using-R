#RIDGE REGRESSION AND LASSO-Model Selection Techniques

require(glmnet)
?glmnet

attach(Hitters)
#Package 'glmnet' does not uses Model Formula language, so we will set up the
#Predictors and Response variable for the Model

x = model.matrix(Salary~.-1 , data = Hitters)#Predictors  
y = Hitters$Salary #Response Variable to be used in Linear Model

#First we will do Ridge Regression by setting alpha=0
#In Ridge Regression difference is that it includes all the variables p
#in the Models and does not includes a subset of variables.
#So in shrinkage methods we will simply shrink the coefficient value towards
#0 


ridge<-glmnet(x, y ,alpha = 0)#Ridge Regression Model
summary(ridge)
ridge
#Plotting lambda(Tuning Parameter) vs Cofficient values of variables.
plot(ridge,xvar = "lambda",label = TRUE)
plot(ridge,xvar = "dev",label = TRUE)
#Plots the fraction of deviance Explained-similr to R-squared value

#In Shrinkage techniques we will Shrink the Cofficient values towards 0 as
#value of lambda increases to reduce the Error value

#Corss validation with 10 folds
cv.ridge = cv.glmnet(x,y,alpha=0)
plot(cv.ridge)#all 20 variables in the Model-19 predictors + 1 intercept for each
#Plot of MSE on Validation Set vs Lambda(Tuning Parameter)
#Test Error first Decreases as RIDGE REGERSSION decreases the Model's variance
#and complexity but after a point as lambda increases the Bias increases a
#lot which causes Underfitting and again Error goes up.
#With increase in Lambda the variance of Model decreases at cost of High Bias



#LASSO-Another shrinkage Technique which Does Variable Selection as well as
#shrinks the cofficient value towards 0 and sometimes excatly 0 which 
#reduces the Complexity of the Models , The best part is that it selects
#a subset of variables for the Model , unlike RIDGE REGRESSION
#It prefers Sparse Models

#by default alpha=  0
lasso<-glmnet(x,y)
lasso
plot(lasso,xvar ="lambda",label = T)
plot(lasso,xvar = "dev",label = T)
#At the end there is quiet a big jump in coef values, so it might be 
#overfitting

#again using Cross validation
lasso.cv<-cv.glmnet(x,y)
plot(lasso.cv)
#We get least errors at about 14 predictors and withing 1 SE is Model with 
#around 5 predictors
#Finding the coeffienct values for best Model
coef(lasso.cv)#best Model selected is with 5 predictors- within 1 Standard error
# of the minimum 




#Using a Validation Set

#Train on training Set
lasso.val<-glmnet(x[trainrow,],y[trainrow])
plot(lasso.val,xvar = 'dev',label = T)
pred=predict(lasso.val,x[-trainrow,])#Predictions on Test set
pred
dim(pred)
rmse = sqrt(apply((y[-trainrow]- pred)^2 ,2 ,mean))#Root mean squared error
#on Validation Set

plot(log(lasso.val$lambda),rmse,type='b',pch=19,col='red',
     ylab="Root Mean Square Error on Validation Set",xlab=("Log of Lambda(Tuning Parameter)"))
title("Lasso Implementation")
#On the Left side of the plot, when lambda is small - it represents Overfitting,high variance
#on the Right Hand side- when lambda is very large it repssents Underfitting,high Bias
#Somewhere in the middle of the plot the Bias Variance is balanced

#Extracting the Best value of Lambda which gives least Error on Validation Set
lam.best<-lasso.val$lambda[order(rmse)[1]]
lam.best
#order in ascending order of rmse and we want the 1st of that list
lam.best

#outputs a sparse matrix format
coef(lasso.val,s=lam.best)
#It outputs a Model with 15 predictors which has least RMSE error on Validation Set