#loading the Splines Packages
require(splines)
#ISLR contains the Dataset
require(ISLR)
attach(Wage)

agelims<-range(age)
#Generating Test Data
age.grid<-seq(from=agelims[1], to = agelims[2])

#3 cutpoints at ages 25 ,50 ,60
fit<-lm(wage ~ bs(age,knots = c(25,40,60)),data = Wage )
summary(fit)
#Plotting the Regression Line to the scatterplot   
plot(age,wage,col="grey",xlab="Age",ylab="Wages")
points(age.grid,predict(fit,newdata = list(age=age.grid)),col="darkgreen",lwd=2,type="l")
#adding cutpoints
abline(v=c(25,40,60),lty=2,col="darkgreen")



fit1<-smooth.spline(age,wage,df=16)
plot(age,wage,col="grey",xlab="Age",ylab="Wages")
points(age.grid,predict(fit,newdata = list(age=age.grid)),col="darkgreen",lwd=2,type="l")
#adding cutpoints
abline(v=c(25,40,60),lty=2,col="darkgreen")
lines(fit1,col="red",lwd=2)
legend("topright",c("Smoothing Spline with 16 df","Cubic Spline"),col=c("red","darkgreen"),lwd=2)



#Cross Validation to select lambda(Tuning Parameter) for a Model which Minimizes CV Error.

fit2<-smooth.spline(age,wage,cv = TRUE)
fit2
#It selects $\lambda=6.794596$ it is a Heuristic and can take various values for how rough the function is

plot(age,wage,col="grey")
#Plotting Regression Line
lines(fit2,lwd=2,col="purple")
legend("topright",("Smoothing Splines with 6.78 df selected by CV"),col="purple",lwd=2)

