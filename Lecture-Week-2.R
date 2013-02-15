#'
#' Video 1
#' 
#' https://class.coursera.org/dataanalysis-001/lecture/65
#' 

install.packages('kernlab')

require(kernlab)
require(ggplot2)


data(spam)
dim(spam)

# random selection from the data
set.seed(3435)

trainIndicator <- rbinom(nrow(spam), size=1, prob=0.5)
table(trainIndicator)

trainSpam <- spam[trainIndicator == 1,]
testSpam <- spam[trainIndicator == 0,]

dim(trainSpam)
names(trainSpam)
head(trainSpam)
table(trainSpam$type)

plot(trainSpam$capitalAve ~ trainSpam$type)
plot(log10(trainSpam$capitalAve + 1) ~ trainSpam$type)

qplot(data=trainSpam, x=type, y=capitalAve)
qplot(data=trainSpam, x=type, y=log10(capitalAve+1))


plot(log10(trainSpam[,1:4]+1))

hCluster = hclust(dist(t(trainSpam[,1:57])))
plot(hCluster)

hCluster = hclust(dist(t(log10(trainSpam[,1:55]+1))))
plot(hCluster)

trainSpam$numType <- as.numeric(trainSpam$type)-1

costFunction <- function(x, y) {
  return(sum(x!=(y>0.5)))
}

cvError <- rep(NA, 55)

library(boot)

for (i in 1:length(cvError)) {

  print(sprintf('Executing %d of %d', i, length(cvError)))
  
  lmFormula <- as.formula(paste('numType ~ ', names(trainSpam)[i], sep=''))
  
  glmFit <- glm(lmFormula, family='binomial', data=trainSpam)

  cvError[i] <- cv.glm(trainSpam, glmFit, costFunction, 2)$delta[2]
}


which.min(cvError)
names(trainSpam)[which.min(cvError)]

# use charDollar in a model and train
predictionModel <- glm(numType ~ charDollar, family='binomial', data=trainSpam)

# predict on test set
predictionTest <- predict(predictionModel, trestSpam)

# how did it do?
predictedSpam <- rep('nonspam', dim(testSpam)[1])

# if the value was over 0.5, its spam
predictedSpam[predictionModel$fitted > 0.5] = 'spam'

table(predictedSpam, testSpam$type)

(61+458)/(1346+456 + 61 + 449)


#
# Organizing Data Analysis
#



















