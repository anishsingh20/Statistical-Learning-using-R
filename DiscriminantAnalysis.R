#CLASSIFICATION ON LINEAR DISCRIMINAT ANALYSIS-A classifier suitable for small data sets
#having low dimenstions and less predictors in input space and also stable for multiclass
#classification K>2(class labels > 2)

#Uses Bayes theoram as a Base Model
require(MASS)

#using LDA on Smarket dataset


#Test Data Frame
Test.2005<-subset(Smarket,Year==2005 , select = c(Lag1,Lag2,Direction))


#Using Previous 2 Days Returns to predict the Direction of Market on The Particular day
#Model trained on Training Data = Inductive Learning
fit1<-lda(Direction ~ Lag1 + Lag2 , data  = Smarket, subset = Year < 2005) 
summary(fit1)
#Perfomance on Training Data
mean(predict(fit1)$class==Smarket$Direction[Year<2005])




#Predictions on TEST DATA SET
fit1.pred<-predict(fit1,newdata = Test.2005)
#returns a list with Classified Label for that data point ,
# Probabilities of each class -Here 'Up' and 'Down' & Discriminant Score

#creating a Data frame of predictions
df<-data.frame(fit1.pred)

#If predictors are Quantitative variables then we classify test points to the class label
#having higher Densities(Prior Probability) | Higher Conditional Probability(Pr(Y|X=xi))

head(df)

#Confusion Matrix for Model's Perfomance

table(predicted=df$class,True=Test.2005$Direction)
#Accuracy rate of 56% , same as that of Logistic Regression Model3

#56% correct classifications , and 44% misclassifications




#MODEL2 - QUADRATIC DISCRIMINANT ANALYSIS(More Complex Due to different Covariance 
#matrix for each class label K)

fit2<-qda(Direction ~ Lag1 + Lag2  , data  = Smarket, subset = Year < 2005)
fit2

#Perfomance on Training Data
t1<-table(Predicted=predict(fit2)$class, True = subset(Smarket,Year < 2005)$Direction )
mean(predict(fit2)$class==Smarket$Direction[Year<2005])

#Predictions on TEST DATA
fit2.pred<-predict(fit2,newdata = Test.2005)
df2<-data.frame(fit2.pred)

table(Predicted = df2$class,True=Test.2005$Direction)

mean(df2$class==Test.2005$Direction)
#Hence The overall accuracy on TEST Data has increased to 60%-better Than LDA Model(fit1)


#Hence Overall accuracy of QDA is higher than LDA
#As QDA is much more complex and complicated than LDA due to the quadratic terms 
#in the formula , although the Training Error for both are Same , but 
#Generalization accuracy for QDA is higher = 60% , for LDA = 56%


