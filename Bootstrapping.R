#Implementation of Resampling technique -Bootstrapping 

require(ISLR)# package which has the datasets used in the demonstration
require(boot)

#alpha=  VAR(Y) - COV(X,Y) / VAR(X) + VAR(Y) - 2COV(X,Y)

#writing a function to compute the alpha


alpha=function(x,y) 
{
  vx = var(x)
  vy = var(y)
  cxy = cov(x,y)
  (vy-cxy)/ (vx + vy - 2*cxy)
  #last line will be printed to console
  
}
#let's test the function out
?Portfolio
plot(Portfolio$X, Portfolio$Y)

alpha(Portfolio$X,Portfolio$Y)

#Now we will use Bootstrap to calculate the standard error of alpha
alpha.se<-function(data,index)
{
  with(data[index,],alpha(X,Y))
  
}
#Now Bootstrap will create re-samples form Original dataset with replacement
#and compute the Standard error for alpha from all those bootstrap data sets


alpha.se(Portfolio,1:100)

set.seed(1) # for reproducable results
alpha.se(Portfolio,sample(1:100,100,replace = TRUE))


boot.out = boot(data = Portfolio,alpha.se,R=1000)#1000 bootstrap samples
boot.out
plot(boot.out,main="Overview for the Bootstrapping applied")
#We are only interested in finding the Standard error of the alpha
# BOTH THE CURVES LOOK PRETTY NORMALLY DISTRINUTED OR MAYBE GAUSSIAN



#now using the output of a bootstrap to find out confidence intervals for alpha
boot.conf<-boot.ci(boot.out,conf = 0.95, type='all')
boot.conf




#hence this is how we can use Bootstrapping to compute the Standard errors, 
#confidence intervals of various statistics and learning techniques