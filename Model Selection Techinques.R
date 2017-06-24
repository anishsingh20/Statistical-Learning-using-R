#MODEL SELECTION in R


require(ISLR)
require(dplyr)
#Major League Baseball Data from the 1986 and 1987 seasons
summary(Hitters)
str(Hitters)
?Hitters

#Removing the Missing values
Hitters<-na.omit(Hitters)
attach(Hitters)



#Best Subset Selection-Model selection procedure which searches for all possible Models 
#and the best Model amongst those Models with 0 predictors to Model with all predictors
#with the search space equal to 2^p , p = no of  predictors
#Amongst those Models the Best one is selected with least AIC or BIC values
#or highest adjusted R-squared value or least CV error.
install.packages("leaps")
#Package to do Subset Selection
require(leaps)
?leaps

#Model selection by exhaustive search, forward or backward stepwise, or sequential replacement
reggfit.full<-regsubsets(Salary ~ . , data = Hitters )
reggfit.full
summary(reggfit.full)->Modsumm
#by default it goes upto only 8 subsets of Predictors and 8 Models only
#Models are not Nested like in Forawrd Stepwise selection

#Residual Sum of squares for each Model
Modsumm$adjr2
plot(reggfit.full,scale = 'Cp')



#Best Subset selection with all 19 variables
Mod2<-regsubsets(Salary ~ . , data = Hitters , nvmax = 19)
summod2<-summary(Mod2)
summod2
plot(Mod2,scale="Cp",xlab="Variables" , ylab =" Cp Statistic, lesser the better Model")
par(mfrow=c(1,2))

#plot of CP vs number of predictors in Model
plot(summod2$cp,xlab="Number of Variables", ylab = "Cp statistic-Lesser The better")
which.min(summod2$cp)#Model with 10 predictors has least Cp
points(10,summod2$cp[10],pch=20,col='blue')
#coloring the Best Model with 10 predictors

#Plot of Adjusted R squared vs number of predictors
plot(summod2$adjr2,xlab="Number of Variables", ylab = "Adjusted R-squared,Larger the better")
which.max(summod2$adjr2)

#To find the Coefficients of the Model selected with index 10
coef(Mod2,id = 10)








#FORWARD STEPWISE SELECTION
forwmod<-regsubsets(Salary ~ . , data  = Hitters , method = 'forward',nvmax = 19 )

sumfor<-summary(forwmod)
#now the models are prefectly Nested-i.e the new Model is evaluated by adding 1 more variable
#to the previous Model with K predictors upto model with all p predictors and the search space
# is p^2 models.
sumfor$cp #cp statistic- the lesser the better
sumfor$bic #BIC statistic-the lesser the better-BIC penalizes larger Models with more predictors  

sumfor$adjr2
which.max(sumfor$adjr2) #model with 11 predictors has highest Adjst R-squared


plot(sumfor$adjr2,xlab="Number of Variables" , ylab =  " Adjusted R-squared")
points(11,sumfor$adjr2[11],pch=20,col='blue')
#Adjst R-squred is highest for Model with 11 predictors
title("Forward Stepwise Selection")
plot(sumfor$bic,xlab='Number of Predictors '  , ylab = "BIC statistic")
points(6,sumfor$bic[6],pch=20,col='red')
#BIC is least for a model with 6 predictors 






#BACKWARD STEPWISE SELECTION
backmod<-regsubsets(Salary ~ . , data  = Hitters , method = 'back', nvmax = 19 )
backsum<-summary(backmod)
backsum


which.min(backsum$rss)
#RSS least for model with all predictors in it i.e 19 , as expected
#as we add more and more variables in the Model RSS value decreases but we cannot select
#that model because it is certainly Overfitting on training data and has very high variance

which.max(backsum$adjr2)
which.min(backsum$cp)
which.min(backsum$bic)#BIC penelizes larger Models

plot(sumfor$adjr2,xlab="Number of Variables" , ylab =  " Adjusted R-squared")
points(11,sumfor$adjr2[11],pch=20,col='green')
#Adjst R-squred is highest for Model with 11 predictors
title("Backward Stepwise Selection")
plot(sumfor$bic,xlab='Number of Predictors '  , ylab = "BIC statistic")
points(8,sumfor$bic[8],pch=20,col='yellow')
#BIC is least for a model with 8 predictors 

#Mean squared error on Training data reduces as  model's complexity increases
# as expected 
#sometimes we have to clear the memory to make things work
#becasue R first loads everything in RAM then executes it

par(mfrow=c(2,2))

#PLOT OF COMPARISON OF VARIOUS STATISTICS TO SELECT THE BEST MODEL AND ALSO SHOWING 
# HOW MSE on TRAINING DATA DECREASES due to INCREASE IN MODEL COMPLEXITY AND VARIANCE  

plot(backsum$rss/nrow(Hitters),type='b',pch=19,xlab="Number of Variables",
     ylab = "Mean Squared Error on Training Data")
title("Overfitting Cases and increase in Model Variance as no of Predictors Increases")
plot(backsum$bic,type='b',pch=19,col="blue",xlab="Number of Variables",
     ylab="BIC value")
plot(backsum$rsq,type="b",pch=19,col='red',xlab="Number of Variables",
     ylab="R-squared on Training data")

plot(backsum$adjr2,type='b',col='green',pch=19,xlab="Number of Variables",
     ylab=('Adjusted R-squared'))

coef(backmod,id=8)





#Model Selection using a Validation Set

#Making training and test Data set

dim(Hitters)

set.seed(1000)#for reproducable results

#Randomly making Training rows for Training data set
#180 random numbers for 1 to 263
trainrow<-sample(seq(263),180,replace = FALSE)
head(trainrow)
#now training using Forward Stepwise selection
forw<-regsubsets(Salary ~ . ,data=Hitters[trainrow,] , 
                 method = "forward",nvmax =19)


#Validation Error vector
#AS there are 19 Models with 1 to 19 predictors in them
val.error<-rep(NA,19)
?model.matrix() #model.matrix creates a design (or model) matrix

#Test data Set- exculding the observations used by Train data
x.test<-model.matrix(Salary ~ ., data = Hitters[-trainrow,])

for(i in 1:19) {
  coefi = coef(forw , id = i)
  pred = x.test[,names(coefi)]%*%coefi
  #Coef value for for all 19 variables
  val.error[i] = mean((Hitters$Salary[-trainrow]-pred )^2)
  #Mean sqrd Error on Test set for all 19 Models
  
}

val.error
#Plotting the RMStest error for Each Model
plot(sqrt(val.error),pch=19,ylim=c(280,400),type='b',ylab="RMS error",xlab="No of Predictors")
#also adding the Mean RSS omn training data
title("MSE on TEST SET vs MSE on TRAIN SET")
points(sqrt(forw$rss[-1]/180),type='b',pch=19,col='blue')
legend('topright',pch=19,c("MSE on Test Set ","MSE on Training Data"),col=c("black","blue"))
#AS expected the MSE in Training data decrease as the no of predictors
#in the Model Increases due to forward stepwise which adds 1 more Best 
#predictor to the Next model, but due to this the variance of 
#the Models increases with more predictors and it starts
#to Overfit the training data and performs poorly on Test Set.
#Also in the Plot we can see the Train error is least for Models with
# highest TEST ERROR , this is OVERFITTING due to high Model variances



#function to compute Error and predictions on test set
predict.regsubset<-function(object,newdata,id,...)
{
  form=as.formula(object$call[[2]])
  mat<-model.matrix(form,newdata)#Test Set
  coefi<-coef(object,id=id)# Model coefficients
  #last line are the predicted values
  mat[,names(coefi)]%*%coefi
}


#CROSS VALIDATION FOR MODEL SELECTION
#10 fold CV
set.seed(10)
#dividing the data set in 10 equal parts
#random rows for each fold , each fold has same size
folds=sample(rep(1:10,length = nrow(Hitters)))
#repeat 1 to 10 ,263 times(size of data set)
folds
table(folds)

#10 rows of each Model with 19 variables(columns)
cv.errors<-matrix(NA,10,19)

for(k in 1:10)
{
#fitting the Models on each k-1 folds
  best.fit<-regsubsets(Salary ~ . , data = Hitters[folds!=k,],
                       nvmax=19,method='forward')
#predictions on left out Kth fold -Validation Set
for(i in 1:19)
{
  pred = predict.regsubset(best.fit,Hitters[folds==k,],
                           id=i) #predictions on k-th fold
  cv.errors[k,i]=mean((Hitters$Salary[folds==k]-pred)^2)
  #cross validation error for each K fold Model and  its ith submodel
}

}

head(cv.errors)
pred#a list of Predicted Salaries for each player for kth fold
#10 Models in total with 19 SubModels for each Model

#Cross validation Error for each 20  Sub Models
rmse.cv=sqrt(apply(cv.errors,2,mean))
rmse.cv
plot(rmse.cv,pch=19,type='b',ylim=c(320,400),ylab="Root Mean squared Cross validation Error",
     xlab="Number of Predictors")
title("CV error for each Submodel")

#It tends to prefer Model with 10 and 11 predictors as CV error is least for them












