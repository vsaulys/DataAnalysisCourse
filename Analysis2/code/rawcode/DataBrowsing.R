#'
#' Data Analysis Assignment 2
#' 

require (plyr)
require (ggplot2)
require (grid)
require (randomForest)
require (rpart)
require (ipred)

#'
#' Function for helping layout grid graph elements
#' .x X location for plot
#' .y Y location for plot
#' 
vplayout <- function(.x, .y) {
  viewport(layout.pos.row=.x, layout.pos.col=.y)
}

# directory locations
analysisDir <- 'Analysis2'
dataDir <- file.path(analysisDir, 'data')

# read in the data
load(file.path(dataDir, 'samsungData.rda'))

allColumns <- names(samsungData)

# columns to use, onlymeans of measured
modelColumns <- allColumns[grepl('mean', allColumns)]
modelColumns <- modelColumns[grepl('^t', modelColumns)]

modelColumns

# save the columns
write.csv(modelColumns, file.path(dataDir, 'modelColumns.csv'), row.names=FALSE, quote=FALSE)

# some data reworking
samsungData$activity <- factor(samsungData$activity)
summary(samsungData$activity)

coreData <- samsungData[, c(modelColumns, 'activity', 'subject')]

# fix column names for use in formulas
names(coreData) <- gsub("-", ".", names(coreData))
names(coreData) <- gsub("\\(\\)", "", names(coreData))

names(coreData)

# Training set
trainData <- subset(coreData, subject %in% c(1, 3, 5, 6))
trainData2 <- subset(coreData, !subject %in% c(27, 28, 29, 30))

trainList <- which(coreData$subject %in% c(1, 3, 5, 6))
trainList2 <- which(!coreData$subject %in% c(27, 28, 29, 30))

testData <- subset(coreData, subject %in% c(27, 28, 29, 30))

nrow(trainData)
nrow(testData)

# library(rattle)
# rattle()

# classification: randomForest
cols <- names(coreData)
preds <- cols[!cols %in% c('subject', 'activity')]

f <- as.formula(sprintf('activity ~ %s', paste(preds, collapse='+')))

modelRF <- randomForest(formula=f, data=trainData, ntree=500, importance=TRUE, proximity=TRUE, replace=FALSE)
#randomForest::combine(modelRF1, modelRF2, modelRF3)

varImpPlot(modelRF)
plot(modelRF)
dev.new()
MDSplot(modelRF, trainData$activity, k=20)

# predict

aTrain <- subset(trainData, activity == 'laying')
predRF <- predict(modelRF, aTrain)

pred <- prediction(as.numeric(predRF), predRF)
perf <- performance(predRF, measure = "tpr", x.measure = "fpr") 

plot(perf, col=rainbow(10))

#
# package: rpart
# -------------------------------------
modelT <- rpart(formula=f, data=coreData, subset=trainList)
modelTT <- rpart(formula=f, data=coreData, subset=trainList2)

par(mfrow=c(1,1))

printcp(modelT)
plot(modelT)
text(modelT)
plotcp(modelT)
fancyRpartPlot(modelT, main="Decision Tree for Activity")
modelT

predT <- predict(modelT, type="class", newdata=testData)
resT <- predT == as.character(testData$activity)
table(resT)

par(mfrow=c(1,1))

printcp(modelTT)
plot(modelTT)
text(modelTT)
plotcp(modelTT)
fancyRpartPlot(modelTT, main="Decision Tree for Activity")
modelTT

predT <- predict(modelT, type="class", newdata=testData)
resT <- predT == as.character(testData$activity)
table(resT)

resTFrame <- data.frame(Pred=predT, Res=resT)
summary(subset(resTFrame, Res == FALSE))

predTT <- predict(modelTT, type="class", newdata=testData)
resTT <- predTT == as.character(testData$activity)
table(resTT)

resTTFrame <- data.frame(Pred=predTT, Res=resTT)
summary(subset(resTTFrame, Res == FALSE))

# plot the comparison of model complexity
par(mfrow=c(1,2))
plotcp(modelT)
plotcp(modelTT)

# plot the desicion trees
par(mfrow=c(1,2))
fancyRpartPlot(modelT, main="Small Training Set")
fancyRpartPlot(modelTT, main="Large Training Set")



modelTT_P <- prune(modelTT, cp=0.01)
plotcp(modelTT_P)



# predict
predT <- predict(modelT, newdata=testData)
predTT <- predict(modelTT, newdata=testData)

prune(modelT)

mypredict.rpart <- function(object, newdata){
  predict(object, newdata, type = "class")
}

res <- errorest(f, data=coreData, subset=trainList, model = rpart, predict = mypredict.rpart)
res

res2 <- errorest(f, data=coreData, subset=trainList2, model = rpart, predict = mypredict.rpart)
res2





x1.rp <- rpart(f, data=trainData)

# predict classes for the evaluation data set
x1.rp.pred <- predict(x1.rp, type="class", newdata=testData)

# score the evaluation data set (extract the probabilities)
x1.rp.prob <- predict(x1.rp, type="prob", newdata=testData)

x1.rp.prob.rocr <- prediction(x1.rp.prob[,2], testData$activity)




#
# package: tree
# -------------------------------------

# model perforamnce T
modelT <- tree(formula=f, data=coreData, subset=trainList)
cv1 <- cv.tree(modelT,FUN=prune.tree,method="misclass")
cv2 <- cv.tree(modelT)

par(mfrow=c(1,2))
plot(cv1)
plot(cv2)

# model performance TT
modelTT <- tree(formula=f, data=coreData, subset=trainList2)
cv1 <- cv.tree(modelTT,FUN=prune.tree,method="misclass")
cv2 <- cv.tree(modelTT)

par(mfrow=c(1,2))
plot(cv1)
plot(cv2)



cols <- names(coreData)
preds <- cols[!cols %in% c('subject', 'activity')]

for (aPred in preds) {
  print(ggplot(coreData) + geom_histogram(aes_string(x=aPred)))
}

