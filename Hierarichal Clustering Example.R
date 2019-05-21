#Author :Ashutosh kawle
#Date : 30/04/2019

#this programme explains working of herarichal clustering technique.

# Perform Principal component analysis and perform clustering using first 
#  3 principal component scores (both heirarchial and k mean clustering(scree plot or elbow curve) and obtain 
#  optimum number of clusters and check whether we have obtained same number of clusters with the original data 
#  (class column we have ignored at the begining who shows it has 3 clusters)df
                              
#data set used : wine.csv

#gettting working directory 
getwd() #"C:/Users/Ashutosh/Documents"
setwd("D:\\Data Science Excel R\\DataScience Assignments\\PCA") # setting working directory to file location
getwd()

winedata <- read.csv("wine.csv")
head(winedata)
getWineTyps <- factor(winedata$Type)
levels(getWineTyps) 
getWineData <- winedata[,-1] #removing the fisrt column as it is a type column 
head(getWineData)

#normalizing the Wine data 

getwinedata_normalize <- scale(getWineData)
head(getwinedata_normalize)
?dist
#find the distnace matrix 
d_euclidean <- dist(getwinedata_normalize,method ="euclidean" ) #euclidean distance matrix
d_maximum <- dist(getwinedata_normalize, method ='manhattan') # manhattan distance matrix
?hclust
# agglomeration method to be used. 
# This should be (an unambiguous abbreviation of) one of "ward.D",
# "ward.D2", "single", "complete", "average" 
# (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC).

#creating clsuter with single linkage :
fit_single <- hclust(d_euclidean,method ="single")
plot(fit_single)

fit_single_manhttan <- hclust(d_maximum,method ="single")
plot(fit_single_manhttan)
plot(fit_single_manhttan, hang = -1)




fit_Complete_manhttan <- hclust(d_maximum,method ="complete")
plot(fit_Complete_manhttan)
plot(fit_Complete_manhttan, hang = -1) # get better visualization here 


fit_average_manhttan <- hclust(d_maximum,method ="average")
plot(fit_Complete_manhttan)
plot(fit_Complete_manhttan, hang = -1) # get better visualization here 

#creating 3 clusters using fit_complete _manhattan 

groups <- cutree(fit_Complete_manhttan,k = 3)
memebership <- as.matrix(groups)
rect.hclust(fit_Complete_manhttan,k=3,border = 'Red')
winedataFinal <- data.frame(memebership,getWineData)


head(winedataFinal)
head(winedata)
confusion <- table(winedata$Type,winedataFinal$memebership) # to compare the original groups and groups created after clsuetring

#creating cluster using avaerage linkage manhatten

groups <- cutree(fit_average_manhttan,k = 3)
memebership <- as.matrix(groups)
rect.hclust(fit_average_manhttan,k=3,border = 'Red')
winedataFinal2 <- data.frame(memebership,getWineData)


head(winedataFinal2)
head(winedata)
confusion <- table(winedata$Type,winedataFinal2$memebership)

confusion



#creating clusters using K means metod 
#non hierarichal tehnique :
#install.packages("plyr")
library(plyr)

#rumming km with winedata 
kmWine <- kmeans(getwinedata_normalize,3)
str(kmWine)
kmWine_final <- data.frame(getWineData,kmWine$cluster)
confusion <- table(kmWine_final$kmWine.cluster,winedata$Type)
#kmWine$withinss
wss <- NULL
#elbow curve to get no. of clusters to be choosen
for (i in (1:28)){
  wss[i] = sum(kmeans(getwinedata_normalize,centers = i)$withinss)
}
wss

plot(1:28,wss,type="b",xlab = "no. of clusters", ylab = "within a group sum of squares")

kmWine <- kmeans(getwinedata_normalize,3)
kmWine_final4 <- data.frame(getWineData,kmWine$cluster)
aggregate(getWineData,by=list(kmWine_final4$kmWine.cluster),FUN = mean)

# k=4 is the ideal cluster ...
kmWine <- kmeans(getwinedata_normalize,4)
kmWine_final4 <- data.frame(getWineData,kmWine$cluster)
aggregate(getWineData,by=list(kmWine_final4$kmWine.cluster),FUN = mean)

kmWine <- kmeans(getwinedata_normalize,5)
kmWine_final4 <- data.frame(getWineData,kmWine$cluster)
aggregate(getWineData,by=list(kmWine_final4$kmWine.cluster),FUN = mean)


# performing dimension reduction tecnique -1
# PCA code
#install.packages("gdata") #ignore this pacjage as it is useful for excel operation we can use reaxlsx
#library(gdata)

pcaobj <- princomp(getWineData,cor = TRUE,scores = TRUE, covmat = NULL)
summary(pcaobj)#first 9 rows conttain 94% of information can be used for analysis
loadings(pcaobj)
plot(pcaobj)
biplot(pcaobj)
nrow(pcaobj$scores) 
nrow(pcaobj$scores)


