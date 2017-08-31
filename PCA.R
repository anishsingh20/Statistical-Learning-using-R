#unsupervised learning -PCA on UDArrests data set

#we will use USAarrests data

?USArrests
#dataset which contains Violent Crime Rates by US State
dim(USArrests)
dimnames(USArrests)

#finding mean of all 
apply(USArrests,2,mean)
apply(USArrests,2,var)
