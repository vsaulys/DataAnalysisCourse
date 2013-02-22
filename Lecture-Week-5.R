#'
#' Week 5 Lectures
#'
#'
#'

require(ggplot2)

#
# Lecture: ANOVA with multiple factors
# -------------------------------------------------------------

download.file("http://www.rossmanchance.com/iscam2/data/movies03RT.txt", destfile="./data/movies.txt")
movies <- read.table("./data/movies.txt",sep="\t",header=T,quote="")
head(movies)

f <- score ~ rating

aovObject <- aov(f, data=movies)
aovObject

aovObject$coefficients

f2 <- score ~ rating + genre
aovObject2 <- aov(f2, data=movies)
aovObject2

summary(aovObject2)

# unbalanced design, order matters!
f3 <- score ~ genre + rating
aovObject3 <- aov(f3, data=movies)
aovObject3
summary(aovObject3)
summary(aovObject2)


f4 <- score ~ genre + rating + box.office
aovObject4 <- aov(f4, data=movies)
aovObject4
summary(aovObject4)

#
# Lecture: Binary Outcomes
# -------------------------------------------------------------

download.file("https://dl.dropbox.com/u/7710864/data/ravensData.rda", destfile="./data/ravensData.rda", method="curl")
load("./data/ravensData.rda")
head(ravensData)

f <- ravenWinNum ~ ravenScore

lmRavens <- lm(f, data=ravensData)
summary(lmRavens)

plot(ravensData$ravenScore,lmRavens$fitted,pch=19,col="blue",ylab="Prob Win",xlab="Raven Score")


logRegRavens <- glm(f, data=ravensData, family='binomial')
summary(logRegRavens)

plot(ravensData$ravenScore,logRegRavens$fitted,pch=19,col="blue",xlab="Score",ylab="Prob Ravens Win")

exp(logRegRavens$coeff)
exp(confint(logRegRavens))

anova(logRegRavens, test='Chisq')

#
# Lecture: Couting Outcomes
# -------------------------------------------------------------

download.file("https://dl.dropbox.com/u/7710864/data/gaData.rda", destfile="./data/gaData.rda", method='curl')
load("./data/gaData.rda")
gaData$julian <- julian(gaData$date)
head(gaData)
              
plot(gaData$julian,gaData$visits,pch=19,col="darkgrey",xlab="Julian",ylab="Visits")
ggplot(data=gaData, aes(x=julian, y=visits)) + geom_point(alpha=.5) + labs(x='Julian', y='Visits')

plot(gaData$julian,gaData$visits,pch=19,col="darkgrey",xlab="Julian",ylab="Visits")
lm1 <- lm(gaData$visits ~ gaData$julian)
abline(lm1,col="red",lwd=3)

              
plot(gaData$julian, gaData$visits, pch=19, col="darkgrey", xlab="Julian", ylab="Visits")
glm1 <- glm(gaData$visits ~ gaData$julian, family="poisson")
abline(lm1,col="red",lwd=3)
lines(gaData$julian, glm1$fitted,col="blue", lwd=3)

plot(glm1$fitted,glm1$residuals,pch=19,col="grey",ylab="Residuals",xlab="Fitted")

library(sandwich)

confint.agnostic <- function (object, parm, level = 0.95, ...) {
  cf <- coef(object); pnames <- names(cf)
  if (missing(parm))
    parm <- pnames
  else if (is.numeric(parm))
    parm <- pnames[parm]
  a <- (1 - level)/2; a <- c(a, 1 - a)
  pct <- stats:::format.perc(a, 3)
  fac <- qnorm(a)
  ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm,
                                                             pct))
  ses <- sqrt(diag(sandwich::vcovHC(object)))[parm]
  ci[] <- cf[parm] + ses %o% fac
  ci
}

confint(glm1)
confint.agnostic(glm1)


glm2 <- glm(simplystats ~ julian(date), offset=log(visits+1), family="poisson", data=gaData)
plot(julian(gaData$date),glm2$fitted,col="blue",pch=19,xlab="Date",ylab="Fitted Counts")
points(julian(gaData$date),glm1$fitted,col="red",pch=19)

glm2 <- glm(simplystats ~ julian(date),offset=log(visits+1), family="poisson", data=gaData)
plot(julian(gaData$date), gaData$simplystats/(gaData$visits+1), col="grey", xlab="Date",
     ylab="Fitted Rates",pch=19)
lines(julian(gaData$date),glm2$fitted/(gaData$visits+1),col="blue",lwd=3)

#
# Lecture: Model checking and selection
# -------------------------------------------------------------

set.seed(3433); par(mfrow=c(1,2))
data <- rnorm(100,mean=seq(0,3,length=100),sd=seq(0.1,3,length=100))
lm1 <- lm(data ~ seq(0,3,length=100))
plot(seq(0,3,length=100),data,pch=19,col="grey")
abline(lm1,col="red",lwd=3)
plot(seq(0,3,length=100),lm1$residuals,,pch=19,col="grey")
abline(c(0,0),col="red",lwd=3)

set.seed(3433)
par(mfrow=c(1,2))
data <- rnorm(100,mean=seq(0,3,length=100),sd=seq(0.1,3,length=1))
                                                                                     
lm1 <- lm(data ~ seq(0,3,length=100))
vcovHC(lm1)
summary(lm1)$cov.unscaled

set.seed(3433)
par(mfrow=c(1,2))
data <- rnorm(100,mean=seq(0,3,length=100)^3,sd=2)
lm1 <- lm(data ~ seq(0,3,length=100))
plot(seq(0,3,length=100),data,pch=19,col="grey")
abline(lm1,col="red",lwd=3)
plot(seq(0,3,length=100),lm1$residuals,,pch=19,col="grey")
abline(c(0,0),col="red",lwd=3)

set.seed(3433); par(mfrow=c(1,3))
z <- rep(c(-0.5,0.5),50)
data <- rnorm(100,mean=(seq(0,3,length=100) + z),sd=seq(0.1,3,length=100))
lm1 <- lm(data ~ seq(0,3,length=100))
plot(seq(0,3,length=100),data,pch=19,col=((z>0)+3))
abline(lm1,col="red",lwd=3)
plot(seq(0,3,length=100),lm1$residuals,pch=19,col=((z>0)+3))
abline(c(0,0),col="red",lwd=3)
boxplot(lm1$residuals ~ z,col = ((z>0)+3) )

set.seed(343)
par(mfrow=c(1,2))
betahat <- rep(NA,100)
x <- seq(0,3,length=100)
y <- rcauchy(100)
lm1 <- lm(y ~ x)

plot(x,y,pch=19,col="blue")
abline(lm1,col="red",lwd=3)
for(i in 1:length(data)){betahat[i] <- lm(y[-i] ~ x[-i])$coeff[2]}
plot(betahat - lm1$coeff[2],col="blue",pch=19)
abline(c(0,0),col="red",lwd=3)

require(MASS)

set.seed(343)
x <- seq(0,3,length=100)
y <- rcauchy(100);
lm1 <- lm(y ~ x)
rlm1 <- rlm(y ~ x)
lm1$coeff
rlm1$coeff

# pg 14
par(mfrow=c(1,2))
plot(x,y,pch=19,col="grey")
lines(x,lm1$fitted,col="blue",lwd=3)
lines(x,rlm1$fitted,col="green",lwd=3)
plot(x,y,pch=19,col="grey",ylim=c(-5,5),main="Zoomed In")
lines(x,lm1$fitted,col="blue",lwd=3)
lines(x,rlm1$fitted,col="green",lwd=3)

# pg 15
set.seed(343)
par(mfrow=c(1,2))
x <- seq(0,3,length=100)
y <- rnorm(100)
lm1 <- lm(y ~ x)
plot(lm1)

# pg 20
download.file("http://www.rossmanchance.com/iscam2/data/movies03RT.txt",destfile="./data/movies.txt", method='curl')
movies <- read.table("./data/movies.txt",sep="\t",header=T,quote="")
head(movies)

# pg 21
movies <- movies[,-1]
lm1 <- lm(score ~ .,data=movies)
aicFormula <- step(lm1)

aicFormula

# pg 23
library(leaps)

regSub <- regsubsets(score ~ ., data=movies)
plot(regSub)

# pg 24
library(BMA)

bicglm1 <- bic.glm(score ~., data=movies, glm.family='gaussian')
print(bicglm1)

















              
              



