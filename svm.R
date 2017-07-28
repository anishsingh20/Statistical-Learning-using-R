#Support Vector Machines in R
set.seed(10023)
#generating data
#a matrix with 20 rows and 2 columns
x=matrix(rnorm(40),20,2)
x
y=rep(c(-1,1),c(10,10))
x[y==1,]=x[y==1,]+1 #2 classes are [-1,1]

#plotting the points
plot(x,col=y+2,pch=19)


#First Making Grids using a function
make.grid<-function(x,n=75) {
  grange=apply(x,2,range)
  x1=seq(from=grange[1,1],to=grange[2,1],length=n)
  x2=seq(from=grange[1,2],to=grange[2,2],length=n)
  expand.grid(X1=x1,X2=x2) #it makes a Lattice for us
}
xgrid=make.grid(x) #is a 75x75 matrix

#now predicting on this new Test Set
ygrid=predict(svm,xgrid)

#plotting the Linear Separator
plot(xgrid,col=c("red","blue")[as.numeric(ygrid)],pch=19,cex=.2)
#creates 2 regions
points(x,col=y+3,pch=19) #adding the points on Plot
points(x[svm$index,],pch=5,cex=2) #Highlighting the Support Vectors


require(e1071)
#converting to a data frame
data<-data.frame(x,y=as.factor(y))
head(data)

svm<-svm(y ~ .,data=data,kernel="linear",cost=10,scale = F)
#here cost 'c' is a tuning parameter .The larger it is more stable the margin becomes, it is like a Regularization parameter 
svm
svm$index #gives us the index of the Support Vectors
#so we have 10 support vectors
svm$fitted #to find the fitted values

#Confusion Matrix of Fitted values and Actual Response values
table(Predicted=svm$fitted,Actual=y)

#accuracy on Training Set
mean(svm$fitted==y)*100 #has 80 % accuracy on Training Set

#plotting
plot(svm,data)



beta = drop(t(svm$coefs)%*%x[svm$index,])
beta


beta0=svm$rho
beta0 #the intercept value
#again Plotting
plot(xgrid,col=c("red","blue")[as.numeric(ygrid)],pch=19,cex=.2)
#creates 2 regions
points(x,col=y+3,pch=19) #adding the points on Plot
points(x[svm$index,],pch=5,cex=2)
abline(beta0/beta[2],-beta[1]/beta[2],lty=1)#is the Decision boundary or Plane
#below are for adding the soft margins
abline((beta0-1)/beta[2],-beta[1]/beta[2],lty=2)
abline((beta0+1)/beta[2],-beta[1]/beta[2],lty=2)