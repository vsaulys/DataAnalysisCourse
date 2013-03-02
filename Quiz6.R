#
# Quiz: Week 6

# Question 3

library(ElemStatLearn)

data(SAheart)

set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

names(SAheart)
glmf <- chd ~ age + alcohol + obesity + tobacco + typea + ldl
glm1 <- glm(glmf, family="binomial", data=trainSA)

summary(glm1)

missClass <- function(values,prediction){
  sum(((prediction > 0.5)*1) != values)/length(values)
}

trainPred <- predict(glm1, trainSA, type='response')
testPred <- predict(glm1, testSA, type='response')

missClass(trainSA$chd, trainPred)
missClass(testSA$chd, testPred)

# Question: 4

library(pgmm)
library(tree)

data(olive)
olive = olive[,-1]

treeF <- Area ~ Palmitic + Palmitoleic + Stearic + Oleic + Linoleic + Linolenic + Arachidic + Eicosenoic
treeOlive <- tree(treeF , data=olive)

plot(treeOlive)
text(treeOlive)

newdata = as.data.frame(t(colMeans(olive)))
newdata
head(olive)

predict(treeOlive, newdata)

# Question: 5
library(pgmm)

data(olive)
olive = olive[,-1]

newData = data.frame(Palmitic = 1200, Palmitoleic = 120, Stearic=200,Oleic=7000,Linoleic = 900, Linolenic = 32, Arachidic=60,Eicosenoic=6)

# just walk the given tree in the question for values in newdata








