#
# Dimension reduction lecture
#

set.seed(12345)
par(mar=rep(0.2, 4))

dataMatrix <- matrix(rnorm(400), nrow=40)

image(1:10, 1:40, t(dataMatrix)[,nrow(dataMatrix):1])

set.seed(678910)

for(i in 1:40) {
  coinFlip <- rbinom(1, size=1, prob=0.5)
  if (coinFlip) {
    dataMatrix[i,] <- dataMatrix[i,] + rep(c(0,5), each=5)
  }
}

par(mar=rep(0.2,4))
image(1:10, 1:40, t(dataMatrix)[,nrow(dataMatrix):1])

par(mar=rep(0.2,4))
heatmap(dataMatrix)




set.seed(678910)

for(i in 1:40) {
  coinFlip1 <- rbinom(1, size=1, prob=0.5)
  coinFlip2 <- rbinom(1, size=1, prob=0.5)
  
  if (coinFlip1) {
    dataMatrix[i,] <- dataMatrix[i,] + rep(c(0,5), each=5)
  }

  if (coinFlip2) {
    dataMatrix[i,] <- dataMatrix[i,] + rep(c(0,5),5)
  }
}

hh <- hclust(dist(dataMatrix))
dataMatrixOrdered <- dataMatrix[hh$order,]

svd2 <- svd(scale(dataMatrixOrdered))
par(mfrow=c(1,3))
image(t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):1])
plot(rep(c(0,1), each=5), pch=19, xlab='Column',ylab='Pattern 1')
plot(rep(c(0,1), 5), pch=19, xlab='Column',ylab='Pattern 2')

svd2 <- svd(scale(dataMatrixOrdered))
par(mfrow=c(1,3))
image(t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):1])
plot(svd2$v[,1], pch=19, xlab='Column',ylab='First right singular vector')
plot(svd2$v[,2], pch=19, xlab='Column',ylab='Second right singular vector')

svd1 <- svd(scale(dataMatrixOrdered))
par(mfrow=c(1,2))
plot(svd1$d, xlab='Column', ylab='Singular value', pch=19)
plot(svd1$d^2/sum(svd1$d^2), xlab='Column', ylab='Percent of variance explained', pch=19)

library(impute)

dataMatrix2 <- dataMatrixOrdered
dataMatrix2[sample(1:100, size=40, replace=F)] <- NA
dataMatrix2 <- impute.knn(dataMatrix2)$data
svd1 <- svd(scale(dataMatrixOrdered))
svd2 <- svd(scale(dataMatrix2))

par(mfrow=c(1,2))
plot(svd1$v[,1], pch=19)
plot(svd2$v[,1], pch=19)




