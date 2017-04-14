# LINEAR REGRESSION STATSLEARN



library(MASS)

install.packages('ISLR')
library(ISLR)
#package for Datasets

library(ggplot2)

names(Boston)


#PART-1 DESCRIPTIVE ANALYSIS OF DATA- using Mean,medians,summaries, plotting and Visualizations

#Plotting Data first -finding relations b/w the variables in the Dataset and analysing those variables
# and knowing the dataset inside out 
?Boston
plot1<-ggplot(aes(x = lstat, y = medv),data = Boston) + 
  geom_point() + 
    geom_smooth(method = 'lm')
#inverse relation b/w the variables-as the lower status population % increases the Median sallaries decreases



#PREDICTIVE MODELLING -PART2

# LINEAR MODEL1
mod1<-lm(medv ~ lstat , data = Boston)

summary(mod1)
#Significant p-values and t-values showing a negetive relation b/w X and Y
plot(lstat ~ medv ,data = Boston)
abline(mod1,col='red' ) #fitting the model to the Plot


#Model components such as residuals , fitted Y values etc 
names(mod1)
summary(resid(mod1))
#RESIDUALS SHOULD ALWAYS BE NORMALLY DISTRIBUTED I.E BELL SHAPED
hist(resid(mod1))
#USING GGPLOT2 syntax
ggplot(aes(x = residuals(mod1)),data = mod1)+ 
         geom_histogram(binwidth=5)


#confidence intervals for each regression coefficients
confint(mod1)

#Predictions and Genrelizations
predict(mod1 , data.frame(lstat = c(5,30,10)))
BIC(mod1)

par(mfrow=c(2,2))
#plotting the Linear model
plot(mod1)


#Multiple Regression
mod2<-lm(medv ~ ., data =Boston) 
#including all the sttr as predictors
#Backward Model Selection technique


mod2
summary(mod2)
#Age ,indus variable not significant when all variables included which says tha
# there is correlations of these variables with other variables

AIC(mod2,mod1)

#Updatiing the Model- and removing irrelevetn features(inputs)
mod3<-update(mod2, ~. - age - indus)
summary(mod3)

#interaction between variables
mod4<-lm(medv ~ lstat*age,data = Boston)
summary(mod4)

#non-linear Models
mod5<-lm(medv ~ lstat + I(lstat^2),data = Boston); summary(mod5)

#Plotting The non-linear Models
attach(Boston)
par(mfrow=c(1,1))
plot(medv~lstat)
#plotting the regression line on the scatterplot-cannot use abline now
points(lstat , fitted(mod5), col='blue', pch=20)

#Another method of polynomial regression using poly() function
mod6<-lm(medv~poly(lstat,5))
summary(mod6)
#This model is more complicated and flexible due to higher degree and has lesser training Error

points(lstat , fitted(mod6),col= 'red', pch=20)


#writing a R function

regplot<-function(x,y,...) { 
  
  plot(x,y,...)
  #linear regression Model
  mod<-lm(y~x)
  #to fit regression line to the scatterplot
  abline(mod,col='red')
  
}



