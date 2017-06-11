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



