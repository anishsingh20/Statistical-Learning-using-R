#Cross validation and Resampling Techniques 

require(ISLR)

#boot package for the Cross valiadation
require(boot)
?cv.glm
#this is the K-fold cross validation for the generalized linear models
data("Auto")
attach(Auto)
plot(mpg ,horsepower)
#both have a negetive correlation as mpg increase the HP decreases


#Leave one Out CV-LOOCV
mod1<-glm(mpg ~ horsepower )
summary(mod1)
#delta is the CV error
cv.glm(Auto,mod1)$delta #pretty slow , leavs one data point and fits a model
# on the remaining data points , then tests on the 1 point

#Let's write a formula to compite the Prediction error for LOOCV

loocv<-function(fit) 
  {
  h = lm.influence(fit)$h
  mean((residuals(fit)/(1-h))^2)
#hence it will compute the error , h=diagonal element of the hat matrix
#hat matrix is the operator matrix which produces the least square fit
}


#let's now try out the function
loocv(mod1)
#it will return the CV error directly


#Now lets try out LOOCV for different polynomials(higher degree regression) 
cv.error = rep(0,5)
#cv.error is a empty vector initiaized with 5 as size to collect error of each Model
degree=1:5
for(d in degree)
{
  model<-glm(mpg ~ poly(horsepower,d),data = Auto)
  cv.error[d] = loocv(model)
  
}
#Plot of degree(K) vs  Cross Validation errors for each Different Model with
#different degrees
plot(x = degree , y = cv.error,type='b' ,title="Cross validation error for different degrees",xlab = "Degree",ylab = "Cross validation error")
#Hence Quadratic Model did a very good job with less CV error 
# along with degree 5
