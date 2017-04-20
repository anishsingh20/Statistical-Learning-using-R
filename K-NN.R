#IMPLEMENTING K-NN(LAZY LEARNER) IN R
#K-NN is a lazy learner and a Simple algorithm , but works good most of the times due to 
#the simple  inductive bias it has.

#We find out the closest neighbouring points(Xi) to the Query(test) point 'q' using a 
#Distance metric and classify to that point -Nearest Neighbour in training data,for k=1


#package for k-NN
require(ISLR)
library(class)
library(dplyr)
?knn
#Classifiaction of Smarket Data set
attach(Smarket)

#Training Data Set-consisting of Predictors Lag1 and Lag2
Smarket %>% filter(Year < 2005) %>%  
  select(Lag1,Lag2) ->train_set

#Test Data Set
Smarket %>% filter(Year == 2005) %>%  
  select(Lag1,Lag2) ->test_set


model1<-knn(train_set,test_set ,Direction[Year<2005],k=1)
#model1 returns some class labels for the Test points=test_set using Euclidean distance
head(model1)

#accuracy of the model-CONFUSION MATRIX
table(Predicted=model1,True=Direction[Year==2005])
#True positives and negetives and perfomance of the model
mean(model1==Direction[Year==2005])
#for 1-NN , accuracy is 50% and error=50% , Poor perfomance




#Model2-3-NN
model2<-knn(train_set,test_set ,Direction[Year<2005],k=3)
            
table(Predicted=model2,True=Direction[Year==2005])
#True positives and negetives and perfomance of the model
mean(model2==Direction[Year==2005])           
#accuracy improved to 53%, i.e does slightly better than chance. as Error<50%




#Model3-


model3<-knn(train_set,test_set ,Direction[Year<2005],k=100)

table(Predicted=model3,True=Direction[Year==2005])
#True positives and negetives and perfomance of the model
mean(model3==Direction[Year==2005])   

#NOTE-100 NN gives accuracy of 54%



#Model4- 200-NN
model4<-knn(train_set,test_set ,Direction[Year<2005],k=200)
table(Predicted=model4,True=Direction[Year==2005])
#True positives and negetives and perfomance of the model
mean(model4==Direction[Year==2005])  
#200-NN set gives accuracy of 57%



#MODEL-5, 

model5<-knn(train_set,test_set ,Direction[Year<2005],k=300)
table(Predicted=model5,True=Direction[Year==2005])
#True positives and negetives and perfomance of the model
mean(model5==Direction[Year==2005])  

#300-NN gives accuracy of 61%(highest)