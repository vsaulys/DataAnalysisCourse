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

subset(loansData, Amount.Funded.By.Investors <= 0)
loansData$Amount.Funded.By.Investors[loansData$Amount.Funded.By.Investors <= 0] <- 0
subset(loansData, Amount.Funded.By.Investors <= 0)

# set the reference level for the factors
loansData$FICO.Range <- relevel(factor=loansData$FICO.Range, ref='640-644')

summary(loansData)
summary(loansData$Interest.Rate)
summary(loansData$Debt.To.Income.Ratio)
summary(loansData$Loan.Length)

ggplot(data=loansData, aes(x=Debt.To.Income.Ratio)) + geom_histogram(binwidth=.01)

ggplot(data=loansData, aes(x=Debt.To.Income.Ratio, y=Interest.Rate, color=FICO.Range)) + geom_point()



names(loansData)
summary(loansData)



lm1 <- lm( loansData$Interest.Rate ~ FICO.Range + Amount.Requested + Debt.To.Income.Ratio + Monthly.Income + Loan.Length, data=loansData)
summary(lm1)

anova(lm1)






