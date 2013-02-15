#
#
#

library(datasets)
data(iris)

names(iris)

irisSubset <- iris[,1:4]
dIris <- dist(irisSubset)

hIris <- hclust(dIris)
plot(hIris)

max(cutree(hIris, h=3))

dataDir <- 'data'
fileUrl <- 'https://spark-public.s3.amazonaws.com/dataanalysis/quiz3question4.rda'
destFile <- file.path(dataDir, 'quiz3question4.rda')

download.file(fileUrl, destfile=destFile, method='curl')
load(destFile)

head(dataSet)
plot(dataSet$x, dataSet$y)

km <- kmeans(x=dataSet, centers=2, nstart=100)

names(km)

km$size
plot(dataSet$x,dataSet$y,col=km$cluster,pch=19,cex=2)

library(ElemStatLearn)
data(zip.train)

# Create an image matrix for the 3rd row, which is a 4
im = zip2image(zip.train,3)
image(im)

im8 <- zip2image(zip.train,8)
im18 <- zip2image(zip.train,18)

image(im8)
image(im18)

svd8 <- svd(im8)
svd18 <- svd(im18)

plot(svd8$d^2/sum(svd8$d^2),xlab="Column",ylab="Percent of variance explained 8",pch=19)
plot(svd18$d^2/sum(svd18$d^2),xlab="Column",ylab="Percent of variance explained 18",pch=19)


