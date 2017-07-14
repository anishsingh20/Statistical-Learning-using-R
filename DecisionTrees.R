#Desicion Trees in R ====================
#Requiring Packages

require(ISLR) #package containing data
require(ggplot2)
require(tree)

#Using the Carseats data set 

attach(Carseats)
?Carseats


#Checking the distribution of Sales

ggplot(aes(x = Sales),data = Carseats) + 
  geom_histogram(color="black",fill = 'purple',alpha = 0.6, bins=30) + 
  labs(x = "Unit Sales in Thousands", y = "Frequency")


#Making a Factor variable from Sales

HighSales<-ifelse(Sales <= 8,"No","Yes")
head(HighSales)

#Making a Data frame
Carseats<-data.frame(Carseats,HighSales)

#We will use the tree() function to fit a Desicion Tree
?tree

#Excluding the Sales atrribute
CarTree<-tree(HighSales ~ . -Sales , data = Carseats,split = c("deviance","gini"))
#split argument split	to specify the splitting criterion to use.

CarTree #Outputs a Tree with various Splits at different Variables and Response at Terminals Nodes
#The numeric values within the braces are the Proportions of Yes and No for each split.

#Summary of the Decision Tree
summary(CarTree)

plot(CarTree)
#Adding Predictors as text to plot
text(CarTree ,pretty = 1 )




set.seed(1001)
#A training sample of 250  examples sampled without replacement
train<-sample(1:nrow(Carseats), 250)
#Fitting another Model
tree1<-tree(HighSales ~ .-Sales , data = Carseats, subset = train)
summary(tree1)
#Plotting
plot(tree1);text(tree1)



#Predicting the Class labels for Test set
pred<-predict(tree1, newdata = Carseats[-train,],type = "class")
head(pred)

#Confusion Matrix to check number of Misclassifications
with(Carseats[-train,],table(pred,HighSales))

#Misclassification Error Rate on Test Set
mean(pred!=Carseats[-train,]$HighSales)




#Pruning-----------------


#10 fold CV
#Performing Cost Complexity Pruning
cv.tree1<-cv.tree(tree1, FUN=prune.misclass)
cv.tree1
plot(cv.tree1)
#Deviance minimum for tree size 15 i.e 15 Splits 

prune.tree1<-prune.misclass(tree1,best = 15)
plot(prune.tree1);text(prune.tree1)




pred1<-predict(prune.tree1 , Carseats[-train,],type="class")

#Confusion Matrix
with(Carseats[-train,],table(pred1,HighSales))

#Misclassification Rate
ErrorPrune<-mean(pred1!=Carseats[-train,]$HighSales)
ErrorPrune
#Error reduced to 25 %

