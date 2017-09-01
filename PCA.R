#unsupervised learning -PCA on UDArrests data set

#we will use USAarrests data

?USArrests
#dataset which contains Violent Crime Rates by US State
dim(USArrests)
dimnames(USArrests)

#finding mean of all 
apply(USArrests,2,mean)
apply(USArrests,2,var) #there is a lot of difference in variances of each variable
#in PCA mean does not playes a role , but variance plays a major role in defining PC
#so very large differences in VAR value of a variable will definately dominate the PC.

#So will have to standardize the variables to Unit variance and SD
#It is done via (X -mean(X) / sd(X) )

#using prcomp() function to do so and get PC.


pca.out<-prcomp(USArrests,scale=TRUE)
pca.out
#summary of the PCA
summary(pca.out)
#maximum % of variance is explained by PC1 , and all PCs are mutually uncorrelated. 
names(pca.out)

#Biplot of the Principal Components which explains the variance in data in each direction
#of the variable
biplot(pca.out,scale = 0, cex=0.65)





