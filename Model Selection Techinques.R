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








