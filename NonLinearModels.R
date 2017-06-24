#MODELLING NON LINEARITIES in DATA using various Non linear functions
#The most basic idead behind adding Non linear properties in the Model is by transforming the 
#data or the variables , alomst every trick transforms the variables to Model Non linearitites
#Say Kernel Smoothing techniques  , Splines or Step functions etc


require(ISLR)
attach(Wage)

mod<-lm(wage~poly(age,4),data =Wage)
#Summary of the Model
summary(mod)
#Range of age variable
agelims<-range(age)
#Generating Test Data
age.grid<-seq(from=agelims[1], to = agelims[2])
#Making Predctions on Test data
pred<-predict(mod,newdata = list(age=age.grid),se=TRUE)
#Standard Error Bands- within 2 Standard Deviations
se.tab<-cbind(pred$fit+2*pred$se.fit,pred$fit -  2*pred$se.fit)
plot(age,wage,col="darkgrey")
#Plotting the age values vs Predicted Wage values for those Ages
lines(age.grid,pred$fit,col="blue",lwd=2)
#To plot the Error bands around the Regression Line
matlines(x=age.grid,y=se.tab,lty =2,col="blue")


#This time we will use different basis of polynomials
fit2<-lm(wage ~ age + I(age^2) + I(age^3) + I(age^4),data = Wage)
summary(fit2)

plot(fitted(mod),fitted(fit2),xlab="First Polynomial Model",ylab="Polynomial Model wrapped inside Identity function", main="Fitted values of Both models are exactly same")


#Making Nested Models-i.e Each Next Model includes previous Model and is a special case for previous one
mod1<-lm(wage ~ education , data = Wage)
mod2<-lm(wage ~ education + age,data = Wage)
mod3<-lm(wage ~ education + age + poly(age,2),data = Wage)
mod4<-lm(wage ~ education + age + poly(age,3),data = Wage)
#using anova() function
anova(mod1,mod2,mod3,mod4)
BIC(mod1,mod2,mod3,mod4)


#Logistic Regression Model the Binary Response variable;
logmod<-glm(I(wage > 250 ) ~ poly(age,3),data = Wage , family = "binomial")
summary(logmod)
#doing Predictions
pred2<-predict(logmod,newdata = list(age=age.grid),se=TRUE)
#Standard Error Bands
#a Matrix with 3 columns
#Confidence intervals
se.band<-pred2$fit + cbind(fit=0,lower=-2*pred2$se.fit , upper = 2*pred2$se.fit )
se.band[1:5,]

#comuting the 95% confidence interval for the Fitted Probabilities value
prob.bands = exp(se.band)/ (1 + exp(se.band))
matplot(age.grid,prob.bands,col="blue",lwd = c(2,2,2),lty=c(1,2,2),
        type="l",ylim=c(0,.1),xlab="Age",ylab="Probability Values")

#jitter() function to uniformly add random noise to properly see the densities
points(jitter(age),I(wage > 250)/10 , pch="I",cex=0.5)

