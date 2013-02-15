#
#

require(ggplot2)
require(plyr)

analysisDir <- 'Analysis1'
dataDir <- file.path(analysisDir, 'data')

# read in the data
load(file.path(dataDir, 'loansData.rda'))

# look at the data
str(loansData)
head(loansData)
summary(loansData)
 
names(loansData)

# cleanups to turn some columns into numerics
loansData$Interest.Rate <- as.numeric(sub("%", "", loansData$Interest.Rate))/100
loansData$Debt.To.Income.Ratio <- as.numeric(sub("%", "", loansData$Debt.To.Income.Ratio))/100
loansData$Loan.Length <- as.numeric(sub("months", "", loansData$Loan.Length))

summary(loansData$Interest.Rate)
summary(loansData$Debt.To.Income.Ratio)
summary(loansData$Loan.Length)

qqplot(loansData$Debt.To.Income.Ratio, loansData$Interest.Rate)

qplot(data=loansData, x=Debt.To.Income.Ratio, y=Interest.Rate)

qplot(data=loansData, x=Debt.To.Income.Ratio)
qplot(data=loansData, x=Interest.Rate)





