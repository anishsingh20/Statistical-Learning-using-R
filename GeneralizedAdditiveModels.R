#generalized Additive Models in R

#requiring the Package 
require(gam)
require(ISLR)
attach(Wage)

gam1<-gam(wage~s(age,df=6)+s(year,df=6)+education ,data = Wage)
#in the above function s() is the shorthand for fitting smoothing splines in gam() function
summary(gam1)
#Plotting the Model
par(mfrow=c(1,3))
plot(gam1,se = TRUE)




#logistic Regression Model
gam2<-gam(I(wage >250) ~ s(age,df=4) + s(year,df=4) +education , data=Wage,family=binomial)

plot(gam2,se=T)


#fitting the Additive Regression Model which is linear in Year
gam3<-gam(I(wage >250) ~ s(age,df=4)+ year + education , data =Wage, family = binomial)
plot(gam3)

#anova() function to test the goodness of fit and choose the best Model
#Using Chi-squared Non parametric Test due to Classification Problem and categorial Target
anova(gam2,gam3,test = "Chisq")



lm1<-lm(wage ~ ns(age,df=4) + ns(year,df=4)+ education , data  = Wage)
#ns() is function used to fit a Natural Spline
lm1


#Now plotting the Model

plot.gam(lm1,se=T)
#Hence the Results are same
