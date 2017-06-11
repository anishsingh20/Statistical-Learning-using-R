#RIDGE REGRESSION AND LASSO-Model Selection Techniques

require(glmnet)
?glmnet

attach(Hitters)
#Package 'glmnet' does not uses Model Formula language, so we will set up the
#Predictors and Response variable for the Model

x = model.matrix(Salary~.-1 , data = Hitters)#Predictors  
y = Hitters$Salary #Response Variable to be used in Linear Model
