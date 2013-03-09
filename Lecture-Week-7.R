#
# Lecture: Week 6
#
require(ggplot2)

# Lecture: Smoothing
# ----------------------------------------------------

download.file("https://spark-public.s3.amazonaws.com/dataanalysis/cd4.data", 
              destfile="./data/cd4.data")
cd4Data <- read.table("./data/cd4.data",
                      col.names=c("time", "cd4", "age", "packs", "drugs", "sex",
                                  "cesd", "id")) 
cd4Data <- cd4Data[order(cd4Data$time),]
head(cd4Data)

plot(cd4Data$time, cd4Data$cd4, pch=19, cex=0.1)

ggplot(data=cd4Data, aes(x=time, y=cd4)) + geom_point(alpha=0.4)

plot(cd4Data$time, cd4Data$cd4, pch=19, cex=0.1)
points(mean(cd4Data$time[1:2]), mean(cd4Data$cd4[1:2]), col='blue')
points(mean(cd4Data$time[2:3]), mean(cd4Data$cd4[2:3]), col='blue')

avLen <- 200
aveTime <- aveCd4 <- rep(NA, length(avLen:(dim(cd4Data)[1]-(avLen-1))))

for(i in avLen:(dim(cd4Data)[1]-(avLen-1))) {
  aveTime[i] <- mean(cd4Data$time[(i-(avLen-1)):(i+(avLen-1))])
  aveCd4[i] <- mean(cd4Data$cd4[(i-(avLen-1)):(i+(avLen-1))])
}
plot(cd4Data$time, cd4Data$cd4, pch=19, cex=0.1)
lines(aveTime, aveCd4, col='blue', lwd=4)

filtTime <- as.vector(filter(cd4Data$time,filter=rep(1,200))/200)
filtCd4 <- as.vector(filter(cd4Data$cd4,filter=rep(1,200))/200) 
plot(cd4Data$time,cd4Data$cd4,pch=19,cex=0.1)
lines(filtTime,filtCd4,col="blue",lwd=3)

filtCd4 <- as.vector(filter(cd4Data$cd4, filter=rep(1,4))/4)
filtCd4[2]

ws = 10
tukey = function(x) pmax(1 - x^2,0)^2 
filt= tukey(seq(-ws,ws)/(ws+1))
filt=filt/sum(filt) 
plot(seq(-(ws),(ws)),filt,pch=19)

ws = 100
tukey = function(x) pmax(1 - x^2,0)^2
filt= tukey(seq(-ws,ws)/(ws+1))
filt=filt/sum(filt)
filtTime <- as.vector(filter(cd4Data$time,filter=filt))
filtCd4 <- as.vector(filter(cd4Data$cd4,filter=filt)) 
plot(cd4Data$time,cd4Data$cd4,pch=19,cex=0.1); lines(filtTime,filtCd4,col="blue",lwd=3)

lw1 <- loess(cd4 ~ time, data=cd4Data)
plot(cd4Data$time, cd4Data$cd4, pch=19, cex=0.1)
lines(cd4Data$time, lw1$fitted, col='blue', lwd=3)

plot(cd4Data$time,cd4Data$cd4,pch=19,cex=0.1,ylim=c(500,1500)) 
lines(cd4Data$time,loess(cd4 ~ time,data=cd4Data,span=0.1)$fitted,col="blue",lwd=3) 
lines(cd4Data$time,loess(cd4 ~ time,data=cd4Data,span=0.25)$fitted,col="red",lwd=3) 
lines(cd4Data$time,loess(cd4 ~ time,data=cd4Data,span=0.76)$fitted,col="green",lwd=3)

ggplot(data=cd4Data, aes(x=time, y=cd4)) + geom_point(alpha=0.4) + geom_smooth()

tme <- seq(-2,5,length=100)
pred1 = predict(lw1,newdata=data.frame(time=tme),se=TRUE) 
plot(tme,pred1$fit,col="blue",lwd=3,type="l",ylim=c(0,2500))
lines(tme,pred1$fit + 1.96*pred1$se.fit,col="red",lwd=3)
lines(tme,pred1$fit - 1.96*pred1$se.fit,col="red",lwd=3) 
points(cd4Data$time,cd4Data$cd4,pch=19,cex=0.1)

library(splines)

ns1 <- ns(cd4Data$time, df=3)
par(mfrow=c(1,3))
plot(cd4Data$time,ns1[,1])
plot(cd4Data$time,ns1[,2])
plot(cd4Data$time,ns1[,3])

lm1 <- lm(cd4Data$cd4 ~ ns1) 
summary(lm1)

par(mfrow=c(1,1))

plot(cd4Data$time,cd4Data$cd4,pch=19,cex=0.1) 
points(cd4Data$time,lm1$fitted,col="blue",pch=19,cex=0.5)

# Lecture: Bootstrapping
# ----------------------------------------------------

set.seed(333)
x <- rnorm(30)
bootMean <- rep(NA,1000)
sampledMean <- rep(NA,1000)
for(i in 1:1000){bootMean[i] <- mean(sample(x,replace=TRUE))} 
for(i in 1:1000){sampledMean[i] <- mean(rnorm(30))} 
plot(density(bootMean))
lines(density(sampledMean),col="red")

library(boot)

set.seed(333)
x <- rnorm(30)
sampledMean <- rep(NA,1000) 
for(i in 1:1000){sampledMean[i] <- mean(rnorm(30))} 
meanFunc <- function(x,i){mean(x[i])}
bootMean <- boot(x,meanFunc,1000)
bootMean

plot(density(bootMean$t))
lines(density(sampledMean), col='red')

data(nulcear)
nuke.lm <- lm(log(cost) ~ date,data=nuclear) 
plot(nuclear$date,log(nuclear$cost),pch=19) 
abline(nuke.lm,col="red",lwd=3)

par(mfrow=c(1,3)) 
for(i in 1:3){
  nuclear0 <- nuclear[sample(1:dim(nuclear)[1],replace=TRUE),] 
  nuke.lm0 <- lm(log(cost) ~ date,data=nuclear0) 
  plot(nuclear0$date,log(nuclear0$cost),pch=19) 
  abline(nuke.lm0,col="red",lwd=3)
}


bs <- function(data, indices,formula) {
  d <- data[indices,]
  fit <- lm(formula, data=d)
  return(coef(fit))
}
results <- boot(data=nuclear, statistic=bs, R=1000, formula=log(cost) ~ date) 

par(mfrow=c(1,1)) 

plot(density(results$t[,2]),col="red",lwd=3) 
lines(rep(nuke.lm$coeff[2],10),seq(0,8,length=10),col="blue",lwd=3)

boot.ci(results)

resid <- rstudent(nuke.lm)
fit0 <- fitted(lm(log(cost) ~ 1,data=nuclear)) 
newNuc <- cbind(nuclear,resid=resid,fit0=fit0) 

bs <- function(data, indices) {
  return(coef(glm(data$fit0 + data$resid[indices] ~ data$date,data=data))) 
}

results <- boot(data=newNuc, statistic=bs, R=1000)

plot(density(results$t[,2]),lwd=3,col="blue") 
lines(rep(coef(nuke.lm)[2],10),seq(0,3,length=10),col="red",lwd=3)

B <- dim(results$t)[1]
(1 + sum((abs(results$t[,2]) > abs(coef(nuke.lm)[2]))))/(B+1)

set.seed(555)
x <- rnorm(30)
sampledMed <- rep(NA,1000)

for(i in 1:1000){sampledMed[i] <- median(rnorm(30))}
medFunc <- function(x,i){median(x[i])}
bootMed <- boot(x,medFunc,1000) 
plot(density(bootMed$t),col="red",lwd=3) 
lines(density(sampledMed),lwd=3)

set.seed(333)
x <- rnorm(30)
sampledMax <- rep(NA,1000)
for(i in 1:1000){sampledMax[i] <- max(rnorm(30))}
maxFunc <- function(x,i){max(x[i])}
bootMax <- boot(x,maxFunc,1000) 
plot(density(bootMax$t),col="red",lwd=3,xlim=c(1,3)) 
lines(density(sampledMax),lwd=3) 

# Lecture: Bootstrapping for prediction
# ----------------------------------------------------

library(boot)

data(nuclear)
nuke.lm <- lm(log(cost) ~ date,data=nuclear) 
plot(nuclear$date,log(nuclear$cost),pch=19) 
abline(nuke.lm,col="red",lwd=3)

# bootstrapping prediction errors
newdata <- data.frame(date = seq(65,72,length=100))
nuclear <- cbind(nuclear,resid=rstudent(nuke.lm),fit=fitted(nuke.lm)) 
nuke.fun <- function(data,inds,newdata){
  lm.b <- lm(fit + resid[inds] ~ date,data=data) 
  pred.b <- predict(lm.b,newdata) 
  return(pred.b)
}
nuke.boot <- boot(nuclear,nuke.fun,R=1000,newdata=newdata) 
head(nuke.boot$t)

pred <- predict(nuke.lm,newdata)
predSds <- apply(nuke.boot$t,2,sd) 
plot(newdata$date,pred,col="black",type="l",lwd=3,ylim=c(0,10)) 
lines(newdata$date,pred + 1.96*predSds,col="red",lwd=3) 
lines(newdata$date,pred - 1.96*predSds,col="red",lwd=3)

library(ElemStatLearn)
data(ozone,package="ElemStatLearn") 
ozone <- ozone[order(ozone$ozone),]
head(ozone)

ll <- matrix(NA,nrow=10,ncol=155) 
for(i in 1:10){
  ss <- sample(1:dim(ozone)[1],replace=T)
  ozone0 <- ozone[ss,]
  ozone0 <- ozone0[order(ozone0$ozone),] 
  loess0 <- loess(temperature ~ ozone, data=ozone0, span=0.2) 
  ll[i,] <- predict(loess0, newdata=data.frame(ozone=1:155))
}

plot(ozone$ozone,ozone$temperature,pch=19,cex=0.5) 
for(i in 1:10){lines(1:155,ll[i,],col="grey",lwd=2)} 
lines(1:155,apply(ll,2,mean),col="red",lwd=2)

# bagging trees

data(iris) 
head(iris)

library(ipred)

bagTree <- bagging(Species ~., data=iris, coob=TRUE) 
print(bagTree)

bagTree$mtrees[[1]]$btree

bagTree$mtrees[[2]]$btree

# Random Forests

library(randomForest)

forestIris <- randomForest(Species~ Petal.Width + Petal.Length, data=iris, prox=TRUE) 
forestIris

getTree(forestIris,k=2)

iris.p <- classCenter(iris[,c(3,4)], iris$Species, forestIris$prox) 
plot(iris[,3], iris[,4], pch=21, xlab=names(iris)[3], ylab=names(iris)[4], 
     bg=c("red", "blue", "green")[as.numeric(factor(iris$Species))], 
     main="Iris Data with Prototypes")
points(iris.p[,1], iris.p[,2], pch=21, cex=2, bg=c("red", "blue", "green"))

# combining random forests
forestIris1 <- randomForest(Species~Petal.Width + Petal.Length,data=iris,prox=TRUE,ntree=50) 
forestIris2 <- randomForest(Species~Petal.Width + Petal.Length,data=iris,prox=TRUE,ntree=50) 
forestIris3 <- randomForest(Species~Petal.Width + Petal.Length,data=iris,prox=TRUE,nrtee=50) 
combine(forestIris1,forestIris2,forestIris3)


newdata <- data.frame(Sepal.Length<- rnorm(1000,mean(iris$Sepal.Length), sd(iris$Sepal.Length)),
                      Sepal.Width <- rnorm(1000,mean(iris$Sepal.Width), sd(iris$Sepal.Width)),
                      Petal.Width <- rnorm(1000,mean(iris$Petal.Width), sd(iris$Petal.Width)),
                      Petal.Length <- rnorm(1000,mean(iris$Petal.Length), sd(iris$Petal.Length)))
pred <- predict(forestIris,newdata)


plot(newdata[,4], newdata[,3], pch=21, xlab="Petal.Length",ylab="Petal.Width", 
     bg=c("red", "blue", "green")[as.numeric(pred)],main="newdata Predictions")

?rfcv

# Lecture: Combining Predictors

#library(devtools) 
#install_github("medley","mewo2") 
library(medley)

set.seed(453234)
y <- rnorm(1000)
x1 <- (y > 0)
x2 <- y*rnorm(1000)
x3 <- rnorm(1000, mean=y, sd=1)
x4 <- (y > 0) & (y < 3)
x5 <- rbinom(1000, size=4, prob=exp(y)/(1+exp(y))) 
x6 <- (y < -2) | (y > 2)
data <- data.frame(y=y,x1=x1,x2=x2,x3=x3,x4=x4,x5=x5,x6=x6) 
train <- sample(1:1000,size=500)
trainData <- data[train,]
testData <- data[-train,]

library(tree)
lm1 <- lm(y ~.,data=trainData) 
rmse(predict(lm1,data=testData),testData$y)

tree1 <- tree(y ~.,data=trainData) 
rmse(predict(tree1,data=testData),testData$y)

tree2 <- tree(y~.,data=trainData[sample(1:dim(trainData)[1]),])

combine1 <- predict(lm1,data=testData)/2 + predict(tree1,data=testData)/2 
rmse(combine1,testData$y)

combine2 <- (predict(lm1,data=testData)/3 + predict(tree1,data=testData)/3 + 
               predict(tree2,data=testData)/3)
rmse(combine2,testData$y)

#library(devtools) 
#install_github("medley","mewo2") 
library(medley)
library(e1071) 
library(randomForest)
x <- trainData[,-1] 
y <- trainData$y 
newx <- testData[,-1]

m <- create.medley(x, y, errfunc=rmse)

for (g in 1:10) {
  m <- add.medley(m, svm, list(gamma=1e-3 * g)); 
}

for (mt in 1:2) {
  m <- add.medley(m, randomForest, list(mtry=mt));
}

m <- prune.medley(m, 0.8)
rmse(predict(m,newx),testData$y)

