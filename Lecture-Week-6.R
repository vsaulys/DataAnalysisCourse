#
# Lecture: Week 6
#
require(ggplot2)
        
# Lecture: Prediction Study Design
# ----------------------------------------------------

# Lecture: Cross Validation
# ----------------------------------------------------
set.seed(12345)

x <- rnorm(10)
y <- rnorm(10)
z <- rbinom(10,size=1,prob=0.5) 

plot(x,y,pch=19,col=(z+3))

par(mfrow=c(1,2))
zhat<-(-0.2<y)&(y<0.6) 
plot(x,y,pch=19,col=(z+3))
plot(x,y,pch=19,col=(zhat+3))


set.seed(1233)
xnew <- rnorm(10)
ynew <- rnorm(10)
znew <- rbinom(10,size=1,prob=0.5) 

par(mfrow=c(1,2))
zhatnew <- (-0.2 < ynew) & (ynew < 0.6) 
plot(xnew,ynew,pch=19,col=(z+3))
plot(xnew,ynew,pch=19,col=(zhatnew+3))

y1 <- y[1:5]
x1 <- x[1:5]
z1 <- z[1:5]

y2 <- y[6:10]
x2 <- x[6:10]
z2 <- z[6:10]

zhat2<-(y2<1)&(y2>-0.5)

par(mfrow=c(1,3))
plot(x1,y1,col=(z1+3),pch=19)
plot(x2,y2,col=(z2+3),pch=19)
plot(x2,y2,col=(zhat2+3),pch=19)

# Lecture: Predicting with Regression
# ----------------------------------------------------

data(faithful)
dim(faithful)

set.seed(333)
trainSamples <- sample(1:272,size=(272/2),replace=F)
trainFaith <- faithful[trainSamples,]
testFaith <- faithful[-trainSamples,]
head(trainFaith)

plot(trainFaith$waiting,trainFaith$eruptions,pch=19,col="blue",xlab="Waiting",ylab="Duration")

lm1 <- lm(eruptions ~ waiting, data=trainFaith)
summary(lm1)

plot(trainFaith$waiting,trainFaith$eruptions,pch=19,col="blue",xlab="Waiting",ylab="Duration")
lines(trainFaith$waiting,lm1$fitted,lwd=3)

coef(lm1)[1] + coef(lm1)[2]*80

newdata <- data.frame(waiting=c(80, 90, 100))
predict(lm1,newdata)

par(mfrow=c(1,2))
plot(trainFaith$waiting,trainFaith$eruptions,pch=19,col="blue",xlab="Waiting",ylab="Duration", main='Train')
lines(trainFaith$waiting,predict(lm1),lwd=3)
plot(testFaith$waiting,testFaith$eruptions,pch=19,col="blue",xlab="Waiting",ylab="Duration", main='Test')
lines(testFaith$waiting,predict(lm1,newdata=testFaith),lwd=3)

# Calculate RMSE on training
sqrt(sum((lm1$fitted-trainFaith$eruptions)^2))

# Calculate RMSE on test
sqrt(sum((predict(lm1,newdata=testFaith)-testFaith$eruptions)^2))

pred1 <- predict(lm1,newdata=testFaith,interval="prediction")
ord <- order(testFaith$waiting)
plot(testFaith$waiting,testFaith$eruptions,pch=19,col="blue")
matlines(testFaith$waiting[ord],pred1[ord,],type="l",,col=c(1,2,2),lty = c(1,1,1), lwd=3)

download.file("https://dl.dropbox.com/u/7710864/data/ravensData.rda",
              destfile="./data/ravensData.rda",method="curl")
load("./data/ravensData.rda")
head(ravensData)

glm1 <- glm(ravenWinNum ~ ravenScore,family="binomial",data=ravensData)
par(mfrow=c(1,2))
boxplot(predict(glm1) ~ ravensData$ravenWinNum,col="blue")
boxplot(predict(glm1,type="response") ~ ravensData$ravenWinNum,col="blue")

par(mfrow=c(1,1))

xx <- seq(0,1,length=10)
err <- rep(NA,10)

for(i in 1:length(xx)) {
  err[i] <- sum((predict(glm1,type="response") > xx[i]) != ravensData$ravenWinNum)
}

plot(xx,err,pch=19,xlab="Cutoff",ylab="Error")

# in practice, will use cross validation (train and test sets)

library(boot)

cost <- function(win, pred = 0) mean(abs(win-pred) > 0.5)

glm1 <- glm(ravenWinNum ~ ravenScore,family="binomial",data=ravensData)
glm2 <- glm(ravenWinNum ~ ravenScore,family="gaussian",data=ravensData)

cv1 <- cv.glm(ravensData,glm1,cost,K=3)
cv2 <- cv.glm(ravensData,glm2,cost,K=3)

cv1$delta
cv2$delta

# Lecture: predicting with trees
# ----------------------------------------------------

data(iris)
names(iris)

table(iris$Species)

plot(iris$Petal.Width,iris$Sepal.Width,pch=19,col=as.numeric(iris$Species))
legend(1,4.5,legend=unique(iris$Species),col=unique(as.numeric(iris$Species)),pch=19)

ggplot(data=iris, aes(x=Petal.Width, y=Sepal.Width, color=Species)) + geom_point()

# An alternative is library(rpart)
library(tree)
library(rpart)

tree1 <- tree(Species ~ Sepal.Width + Petal.Width, data=iris)
summary(tree1)

rpart1 <- rpart(Species ~ Sepal.Width + Petal.Width, data=iris)
summary(rpart1)

plot(tree1)
text(tree1)

plot(rpart1)
text(rpart1)

plot(iris$Petal.Width,iris$Sepal.Width,pch=19,col=as.numeric(iris$Species))
partition.tree(tree1,label="Species",add=TRUE)
legend(1.75,4.5,legend=unique(iris$Species),col=unique(as.numeric(iris$Species)),pch=19)

set.seed(32313)
newdata <- data.frame(Petal.Width = runif(20,0,2.5),Sepal.Width = runif(20,2,4.5))
pred1 <- predict(tree1,newdata)
pred1

pred1 <- predict(tree1,newdata,type="class")
plot(newdata$Petal.Width,newdata$Sepal.Width,col=as.numeric(pred1),pch=19)
partition.tree(tree1,"Species",add=TRUE)

data(Cars93, package='MASS')
head(Cars93)

treeCars <- tree(DriveTrain ~ MPG.city + MPG.highway + AirBags +
                   EngineSize + Width + Length + Weight + Price + Cylinders +
                   Horsepower + Wheelbase,data=Cars93)
plot(treeCars)
text(treeCars)

par(mfrow=c(1,2))
plot(cv.tree(treeCars, FUN=prune.tree, method='misclass'))
plot(cv.tree(treeCars))
par(mfrow=c(1,1))

pruneTree <- prune.tree(treeCars, best=4)
plot(pruneTree)
text(pruneTree)

table(Cars93$DriveTrain, predict(pruneTree, type='class'))

table(Cars93$DriveTrain, predict(treeCars, type='class'))






