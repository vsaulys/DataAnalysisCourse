#
# Week 4
#


# Lecture: Clustering Example
#-------------------------------------------
download.file("https://dl.dropbox.com/u/7710864/courseraPublic/samsungData.rda"
              ,destfile="./data/samsungData.rda",method="curl")
load("./data/samsungData.rda")
names(samsungData)[1:12]

table(samsungData$activity)

par(mfrow=c(1,2))

numericActivity <- as.numeric(as.factor(samsungData$activity))[samsungData$subject==1]
plot(samsungData[samsungData$subject==1,1],pch=19,col=numericActivity,ylab=names(samsungData)[1])
plot(samsungData[samsungData$subject==1,2],pch=19,col=numericActivity,ylab=names(samsungData)[2])
legend(150,-0.1,legend=unique(samsungData$activity),col=unique(numericActivity),pch=19)

# try ggplot?

source("http://dl.dropbox.com/u/7710864/courseraPublic/myplclust.R")
distanceMatrix <- dist(samsungData[samsungData$subject==1,1:3])
hclustering <- hclust(distanceMatrix)
myplclust(hclustering,lab.col=numericActivity)

par(mfrow=c(1,2))
plot(samsungData[samsungData$subject==1,10],pch=19,col=numericActivity,ylab=names(samsungData)[10])
plot(samsungData[samsungData$subject==1,11],pch=19,col=numericActivity,ylab=names(samsungData)[11])

source("http://dl.dropbox.com/u/7710864/courseraPublic/myplclust.R")
distanceMatrix <- dist(samsungData[samsungData$subject==1,10:12])
hclustering <- hclust(distanceMatrix)
myplclust(hclustering,lab.col=numericActivity)

svd1 = svd(scale(samsungData[samsungData$subject==1,-c(562,563)]))
par(mfrow=c(1,2))
plot(svd1$u[,1],col=numericActivity,pch=19)
plot(svd1$u[,2],col=numericActivity,pch=19)

plot(svd1$v[,2],pch=19)

maxContrib <- which.max(svd1$v[,2])
distanceMatrix <- dist(samsungData[samsungData$subject==1,c(10:12,maxContrib)])
hclustering <- hclust(distanceMatrix)
myplclust(hclustering,lab.col=numericActivity)        

names(samsungData)[maxContrib]                          

# different from lecture, rowindexes will be different.
kClust <- kmeans(samsungData[samsungData$subject==1,-c(562,563)],centers=6)
table(kClust$cluster,samsungData$activity[samsungData$subject==1])

# now 100 tries
kClust <- kmeans(samsungData[samsungData$subject==1,-c(562,563)],centers=6,nstart=100)
table(kClust$cluster,samsungData$activity[samsungData$subject==1])

# laying, look at above output
plot(kClust$center[2,1:10],pch=19,ylab="Cluster Center",xlab="")
# walking
plot(kClust$center[1,1:10],pch=19,ylab="Cluster Center",xlab="")

# Lecture: Basic least square
# ---------------------------------------

library(UsingR)
data(galton)
par(mfrow=c(1,2))
hist(galton$child,col="blue",breaks=100)
hist(galton$parent,col="blue",breaks=100)

hist(galton$child,col="blue",breaks=100)

hist(galton$child,col="blue",breaks=100)
meanChild <- mean(galton$child)
lines(rep(meanChild,100),seq(0,150,length=100),col="red",lwd=5)

plot(galton$parent,galton$child,pch=19,col="blue")

set.seed(1234)
plot(jitter(galton$parent,factor=2),jitter(galton$child,factor=2),pch=19,col="blue")

plot(galton$parent,galton$child,pch=19,col="blue")
near65 <- galton[abs(galton$parent - 65)<1, ]
points(near65$parent,near65$child,pch=19,col="red")
lines(seq(64,66,length=100),rep(mean(near65$child),100),col="red",lwd=4)

plot(galton$parent,galton$child,pch=19,col="blue")
near71 <- galton[abs(galton$parent - 71)<1, ]
points(near71$parent,near71$child,pch=19,col="red")
lines(seq(70,72,length=100),rep(mean(near71$child),100),col="red",lwd=4)

plot(galton$parent,galton$child,pch=19,col="blue")
lm1 <- lm(galton$child ~ galton$parent)
lines(galton$parent,lm1$fitted,col="red",lwd=3)

plot(galton$parent,galton$child,pch=19,col="blue")
lines(galton$parent, 26 + 0.646*galton$parent)

par(mfrow=c(1,2))
plot(galton$parent,galton$child,pch=19,col="blue")
lines(galton$parent,lm1$fitted,col="red",lwd=3)
plot(galton$parent,lm1$residuals,col="blue",pch=19)
abline(c(0,0),col="red",lwd=3)

# Lecture: Inference Basics
# ---------------------------------------

library(UsingR)
data(galton)

plot(galton$parent,galton$child,pch=19,col="blue")
lm1 <- lm(galton$child ~ galton$parent)
lines(galton$parent,lm1$fitted,col="red",lwd=3)

lm1

newGalton <- data.frame(parent=rep(NA,1e6),child=rep(NA,1e6))
newGalton$parent <- rnorm(1e6,mean=mean(galton$parent),sd=sd(galton$parent))
newGalton$child <- lm1$coeff[1] + lm1$coeff[2]*newGalton$parent + rnorm(1e6,sd=sd(lm1$residuals))
smoothScatter(newGalton$parent,newGalton$child)
abline(lm1,col="red",lwd=3)

set.seed(134325)
sampleGalton1 <- newGalton[sample(1:1e6,size=50,replace=F),]
sampleLm1 <- lm(sampleGalton1$child ~ sampleGalton1$parent)
plot(sampleGalton1$parent,sampleGalton1$child,pch=19,col="blue")
lines(sampleGalton1$parent,sampleLm1$fitted,lwd=3,lty=2)
abline(lm1,col="red",lwd=3)

sampleGalton2 <- newGalton[sample(1:1e6,size=50,replace=F),]
sampleLm2 <- lm(sampleGalton2$child ~ sampleGalton2$parent)
plot(sampleGalton2$parent,sampleGalton2$child,pch=19,col="blue")
lines(sampleGalton2$parent,sampleLm2$fitted,lwd=3,lty=2)
abline(lm1,col="red",lwd=3)

sampleGalton3 <- newGalton[sample(1:1e6,size=50,replace=F),]
sampleLm3 <- lm(sampleGalton3$child ~ sampleGalton3$parent)
plot(sampleGalton3$parent,sampleGalton3$child,pch=19,col="blue")
lines(sampleGalton3$parent,sampleLm3$fitted,lwd=3,lty=2)
abline(lm1,col="red",lwd=3)

sampleLm <- vector(100,mode="list")
for(i in 1:100){
  sampleGalton <- newGalton[sample(1:1e6,size=50,replace=F),]
  sampleLm[[i]] <- lm(sampleGalton$child ~ sampleGalton$parent)
}

smoothScatter(newGalton$parent,newGalton$child)
for(i in 1:100){abline(sampleLm[[i]],lwd=3,lty=2)}
abline(lm1,col="red",lwd=3)

par(mfrow=c(1,2))
hist(sapply(sampleLm,function(x){coef(x)[1]}),col="blue",xlab="Intercept",main="")
hist(sapply(sampleLm,function(x){coef(x)[2]}),col="blue",xlab="Slope",main="")

sampleGalton4 <- newGalton[sample(1:1e6,size=50,replace=F),]
sampleLm4 <- lm(sampleGalton4$child ~ sampleGalton4$parent)
summary(sampleLm4)

hist(sapply(sampleLm,function(x){coef(x)[2]}),col="blue",xlab="Slope",main="",freq=F)
lines(seq(0,5,length=100),dnorm(seq(0,5,length=100),mean=coef(sampleLm4)[2],
                                sd=summary(sampleLm4)$coeff[2,2]),lwd=3,col="red")


par(mfrow=c(1,2))
hist(sapply(sampleLm,function(x){coef(x)[1]}),col="blue",xlab="Intercept",main="")
hist(sapply(sampleLm,function(x){coef(x)[2]}),col="blue",xlab="Slope",main="")

x <- seq(-5,5,length=100)
plot(x,dnorm(x),type="l",lwd=3)
lines(x,dt(x,df=3),lwd=3,col="red")
lines(x,dt(x,df=10),lwd=3,col="blue")

summary(sampleLm4)$coeff

confint(sampleLm4,level=0.95)

par(mar=c(4,4,0,2));plot(1:10,type="n",xlim=c(0,1.5),ylim=c(0,100),
                         xlab="Coefficient Values",ylab="Replication")
for(i in 1:100){
  ci <- confint(sampleLm[[i]]); color="red";
  if((ci[2,1] < lm1$coeff[2]) & (lm1$coeff[2] < ci[2,2])){color = "grey"}
  segments(ci[2,1],i,ci[2,2],i,col=color,lwd=3)
}
lines(rep(lm1$coeff[2],100),seq(0,100,length=100),lwd=3)

sampleLm4$coeff

confint(sampleLm4,level=0.95)

#
# Lecture: P-values
#--------------------------------------------

library(UsingR)
data(galton)

plot(galton$parent,galton$child,pch=19,col="blue")

lm1 <- lm(galton$child ~ galton$parent)
abline(lm1,col="red",lwd=3)

x <- seq(-20,20,length=100)
plot(x,dt(x,df=(928-2)), col='blue', lwd=3, type='l')

arrows(summary(lm1)$coeff[2,3],0.25,summary(lm1)$coeff[2,3],0,col='red', lwd=4)

summary(lm1)

set.seed(9898324)
yValues <- rnorm(10)
xValues <- rnorm(10)

lm2 <- lm(yValues ~ xValues)
summary(lm2)

x <- seq(-5,5,length=100)
plot(x,dt(x,df=(10-2)),col="blue",lwd=3,type="l")
arrows(summary(lm2)$coeff[2,3],0.25,summary(lm2)$coeff[2,3],0,col="red",lwd=4)

xCoords <- seq(-5,5,length=100)
plot(xCoords,dt(xCoords,df=(10-2)),col="blue",lwd=3,type="l")
xSequence <- c(seq(summary(lm2)$coeff[2,3],5,length=10),summary(lm2)$coeff[2,3])
ySequence <- c(dt(seq(summary(lm2)$coeff[2,3],5,length=10),df=8),0)
polygon(xSequence,ySequence,col="red"); polygon(-xSequence,ySequence,col="red")

# p-value is up to  1
# p-value is area under curve left/right of distribution
# e.g. if the value fell in the heart of the distribution, not significant
# if way to a side, its significant

set.seed(8323)
pValues <- rep(NA,100)
for(i in 1:100){
  xValues <- rnorm(100)
  yValues <- 0.2*xValues + rnorm(100)
  pValues[i] <- summary(lm(yValues ~ xValues))$coeff[2,4]
}
hist(pValues,col="blue",main="",freq=F)
abline(h=1,col="red",lwd=3)

summary(lm(galton$child ~ galton$parent))$coeff

#
# Lecture: factor variables
#--------------------------------------------

library(ggplot2)

download.file("http://www.rossmanchance.com/iscam2/data/movies03RT.txt",destfile="./data/movies.txt")
movies <- read.table("./data/movies.txt",sep="\t",header=T,quote="")
head(movies)

plot(movies$score ~ jitter(as.numeric(movies$rating)),col="blue",xaxt="n",pch=19)
axis(side=1,at=unique(as.numeric(movies$rating)),labels=unique(movies$rating))

meanRatings <- tapply(movies$score, movies$rating, mean)
points(1:4,meanRatings,col="red",pch="-",cex=5)

ggplot(data=movies, aes(x=rating, y=score)) + geom_boxplot()

lm1 <- lm(movies$score ~ as.factor(movies$rating))
summary(lm1)

movies$rating <- factor(movies$rating)

lm1 <- lm(formula=score ~ rating, data=movies)
summary(lm1)

plot(movies$score ~ jitter(as.numeric(movies$rating)),col="blue",xaxt="n",pch=19)
axis(side=1,at=unique(as.numeric(movies$rating)),labels=unique(movies$rating))
points(1:4,lm1$coeff[1] + c(0,lm1$coeff[2:4]),col="red",pch="-",cex=5)

confint(lm1)

movies$rating <- relevel(factor(movies$rating), ref='R')

lm2 <- lm(formula=score ~ rating, data=movies)
summary(lm2)
confint(lm2)



movies$rating <- factor(movies$rating)

lm1 <- lm(formula=score ~ rating, data=movies)
summary(lm1)
anova(lm1)


gMovies <- movies[movies$rating=="G",]; xVals <- seq(0.2,0.8,length=4)
plot(xVals,gMovies$score,ylab="Score",xaxt="n",xlim=c(0,1),pch=19)
abline(h=mean(gMovies$score),col="blue",lwd=3); abline(h=mean(movies$score),col="red",lwd=3)
segments(xVals+0.01,rep(mean(gMovies$score),length(xVals)),xVals+0.01,
         rep(mean(movies$score),length(xVals)),col="red",lwd=2)
segments(xVals-0.01,gMovies$score,xVals-0.01,rep(mean(gMovies$score),length(xVals)),col="blue",lwd=2)

aov1 <- aov(formula=score ~ rating, data=movies)
TukeyHSD(aov1)

# Lecture: multiple regression
# ----------------------------------------------

download.file("http://apps.who.int/gho/athena/data/GHO/WHOSIS_000008.csv?profile=text&filter=COUNTRY:*;SEX:",
              destfile="./data/hunger.csv")
hunger <- read.csv("./data/hunger.csv")
hunger <- hunger[hunger$Sex!="Both sexes",]
head(hunger)
str(hunger)

lm1 <- lm(formula= Numeric ~ Year, data=hunger)

ggplot(data=hunger, aes(x=Year, y=Numeric, color=Sex)) + geom_point()


lmBoth <- lm(formula= Numeric ~ Year + Sex, data=hunger)
summary(lmBoth)


lmBoth <- lm(formula= Numeric ~ Year + Sex + Sex*Year, data=hunger)
summary(lmBoth)

# Lecture: Regression in the real world
#---------------------------------------------------------------

download.file("http://apps.who.int/gho/athena/data/GHO/WHOSIS_000008.csv?profile=text&filter=COUNTRY:*;SEX:*","./data/hunger.csv",method="curl")
hunger <- read.csv("./data/hunger.csv")
hunger <- hunger[hunger$Sex!="Both sexes",]
head(hunger)

ggplot(data=hunger, aes(x=Year, y=Numeric, color=WHO.region)) + geom_point()

anova(lm(Year~WHO.region, data=hunger))

anova(lm(Numeric~WHO.region, data=hunger))

ggplot(data=hunger, aes(x=Year, y=Numeric, color=WHO.region)) + 
  geom_point()


download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data","./data/income.incomeData <- read.csv("./data/income.csv",header=FALSE)
              income <- incomeData[,3]
              age <- incomeData[,1]
              
              
              









