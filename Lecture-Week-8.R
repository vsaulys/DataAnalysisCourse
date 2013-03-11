#
# Lecture: Week 8
#
options(digits=4)

#
# Lecture: Multiple Testing
#

#
# Case Study 1: no true positive
# ----------------------------------------------
set.seed(1010093)

pValues <- rep(NA,1000)

for(i in 1:1000){
  y <- rnorm(20)
  x <- rnorm(20)
  pValues[i] <- summary(lm(y ~ x))$coeff[2,4]
}

# Controls false positive rate
sum(pValues < 0.05)

# Controls FWER
sum(p.adjust(pValues,method="bonferroni") < 0.05)

# should not see any significant relationships, was a random set

# Controls FDR
sum(p.adjust(pValues,method="BH") < 0.05)

# Case Study 2
# -------------------------------------------------------

set.seed(1010093)

pValues <- rep(NA,1000)

# now creating a relationship
for(i in 1:1000){
  x <- rnorm(20)
  # First 500 beta=0, last 500 beta=2
  if(i <= 500){y <- rnorm(20)}else{ y <- rnorm(20,mean=2*x)}
  pValues[i] <- summary(lm(y ~ x))$coeff[2,4]
}

trueStatus <- rep(c("zero","not zero"),each=500)
table(pValues < 0.05, trueStatus)

# 24 is about 5% of result

# Controls FWER
table(p.adjust(pValues,method="bonferroni") < 0.05,trueStatus)

# missed 23 cases, but no false positives

# Controls FDR
table(p.adjust(pValues,method="BH") < 0.05,trueStatus)

# discovered all ssignificant results, but saw 13 false positives 

par(mfrow=c(1,2))
plot(pValues,p.adjust(pValues,method="bonferroni"),pch=19)
plot(pValues,p.adjust(pValues,method="BH"),pch=19)

?p.adjust

#
# Lecture: Simulation for model checking
#

# simulating data from a model
set.seed(44333)
x <- rnorm(50)
e <- rnorm(50)
b0 <- 1
b1 <- 2
y <- b0 + b1*x + e

# violating assumptions
set.seed(44333)
x <- rnorm(50)
e <- rnorm(50)
e2 <- rcauchy(50)
b0 <- 1; b1 <- 2
y <- b0 + b1*x + e
y2 <- b0 + b1*x + e2

par(mfrow=c(1,2))
plot(lm(y ~ x)$fitted,lm(y~x)$residuals,pch=19,xlab="fitted",ylab="residuals")
plot(lm(y2 ~ x)$fitted,lm(y2~x)$residuals,pch=19,xlab="fitted",ylab="residuals")

# repeated simulations
set.seed(44333)
betaNorm <- betaCauch <- rep(NA,1000)
for(i in 1:1000){
  x <- rnorm(50)
  e <- rnorm(50)
  e2 <- rcauchy(50)
  b0 <- 1
  b1 <- 2
  y <- b0 + b1*x + e
  y2 <- b0 + b1*x + e2
  betaNorm[i] <- lm(y ~ x)$coeff[2]
  betaCauch[i] <- lm(y2 ~ x)$coeff[2]
}

quantile(betaNorm)

quantile(betaCauch)

# Monte Carlo Error
boxplot(betaNorm,betaCauch,col="blue",ylim=c(-5,5))

# Simulation based on dataset
library(UsingR)
data(galton)
nobs <- dim(galton)[1]

par(mfrow=c(1,2))
hist(galton$child,col="blue",breaks=100)
hist(galton$parent,col="blue",breaks=100)

# Calculating Means, variances
lm1 <- lm(galton$child ~ galton$parent)
parent0 <- rnorm(nobs,sd=sd(galton$parent),mean=mean(galton$parent))
child0 <- lm1$coeff[1] + lm1$coeff[2]*parent0 + rnorm(nobs,sd=summary(lm1)$sigma)

par(mfrow=c(1,2))
plot(galton$parent,galton$child,pch=19)
plot(parent0,child0,pch=19,col="blue")
par(mfrow=c(1,1))

# Simulating more complicate scenarios
library(bootstrap)
data(stamp)

nobs <- dim(stamp)[1]

hist(stamp$Thickness,col="grey",breaks=100,freq=F)
dens <- density(stamp$Thickness)
lines(dens,col="blue",lwd=3)

# A simulation that is too simple
plot(density(stamp$Thickness),col="black",lwd=3)

for(i in 1:10){
  newThick <- rnorm(nobs,mean=mean(stamp$Thickness),sd=sd(stamp$Thickness))
  lines(density(newThick),col="grey",lwd=3)
}

# simplifying from the desntify estimate
plot(density(stamp$Thickness),col="black",lwd=3)

for(i in 1:10){
  newThick <- rnorm(nobs,mean=stamp$Thickness,sd=dens$bw)
  lines(density(newThick),col="grey",lwd=3)
}

# increasing variability
plot(density(stamp$Thickness),col="black",lwd=3)
for(i in 1:10){
  newThick <- rnorm(nobs,mean=stamp$Thickness,sd=dens$bw*1.5)
  lines(density(newThick,bw=dens$bw),col="grey",lwd=3)
}




