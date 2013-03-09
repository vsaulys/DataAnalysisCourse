#
# Quiz 7
#

# Quetion 2

library(splines)
library(medley)

set.seed(53535)
xValues = seq(0,2*pi,length=100)
yValues = rnorm(100) + sin(xValues)

for (i in 1:10) {
  ns1 <- ns(yValues, df=i)
  lm1 <- lm(yValues ~ ns1) 

  print(sprintf('DF: %02d  %.20f', i, rmse(yValues, lm1$fitted)))
}

# question 3
library(simpleboot) 
data(airquality)
attach(airquality)

quantile(airquality$Wind, probs=0.75)

MyQFunc <- function(data) {
  quantile(data, probs=0.75)
}

MyQFunc(airquality$Wind)

set.seed(883833)
bf <- one.boot(airquality$Wind, FUN=MyQFunc, R=1000)
apply(bf$t, 2, sd)


# Question 4
data(Cars93,package="MASS")

newdata = data.frame(Type = "Large",Price = 20)

library(tree)
library(boot)

tree1 <- tree(DriveTrain ~ Price + Type, data=Cars93) 
rmse(predict(tree1,data=testData),testData$y)

tree.fun <- function(data, inds, newdata){
  tree.b <- tree(DriveTrain ~ Price + Type, data=data) 
  pred.b <- predict(tree.b, newdata) 
  return(pred.b)
}
set.seed(7363)
tree.boot <- boot(Cars93, tree.fun, R=3, newdata=newdata) 
head(tree.boot$t)
levels(Cars93$DriveTrain)

# Question 5
library(ElemStatLearn)
library(e1071)

data(vowel.train)
data(vowel.test) 

vowel.train$y <- factor(vowel.train$y)
vowel.test$y <- factor(vowel.test$y)

set.seed(33833)
rf1 <- randomForest(y~., data=vowel.train)
set.seed(33833)
svm1 <- svm(y~., data=vowel.train)

rf.predict <- predict(rf1, newdata=vowel.test, type='class')
svm.predict <- predict(svm1, newdata=vowel.test, type='class')

resFrame <- data.frame(RF=rf.predict, SVM=svm.predict, ACTUAL=vowel.test$y)

resFrame$RF_RES <- resFrame$RF == resFrame$ACTUAL
resFrame$SVM_RES <- resFrame$SVM == resFrame$ACTUAL
resFrame$BOTH_RES <- resFrame$RF_RES & resFrame$SVM_RES

table(resFrame$RF_RES)[1]/nrow(resFrame)
table(resFrame$SVM_RES)[1]/nrow(resFrame)
table(resFrame$BOTH_RES)[1]/nrow(resFrame)



head(resFrame)


