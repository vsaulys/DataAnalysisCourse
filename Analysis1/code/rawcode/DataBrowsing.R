#
#

require(ggplot2)
require(plyr)
require(grid)

vplayout <- function(.x, .y) {
  viewport(layout.pos.row=.x, layout.pos.col=.y)
}

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
#loansData$Loan.Length <- as.numeric(sub("months", "", loansData$Loan.Length))

# clean data, if less than zero, then nothing was funded
loansData$Amount.Funded.By.Investors[loansData$Amount.Funded.By.Investors <= 0] <- 0

# re-factor factor columns
for (aCol in names(loansData)) {
  if (is.factor(loansData[[aCol]])) {
    loansData[aCol] <- factor(loansData[[aCol]])
  }
}

str(loansData)

# NOTE: factors are naturally ordered alphabetically

# set the reference level for the factors
loansData$FICO.Range <- relevel(x=loansData$FICO.Range, ref='640-644')
loansData$Employment.Length <- relevel(x=loansData$Employment.Length, ref='< 1 year')
loansData$Loan.Length <- relevel(x=loansData$Loan.Length, ref='36 months')

summary(loansData)
summary(loansData$Interest.Rate)
summary(loansData$Debt.To.Income.Ratio)
summary(loansData$Loan.Length)

# hitograms of features to use
ggplot(data=loansData, aes(x=Interest.Rate)) + geom_histogram(binwidth=.01)
ggplot(data=loansData, aes(x=Debt.To.Income.Ratio)) + geom_histogram(binwidth=.01)

# all features in one plot
ggplot(data=loansData, aes(x=Debt.To.Income.Ratio, y=Interest.Rate, color=as.numeric(FICO.Range) ) ) + 
  geom_point() + 
  facet_wrap(~ Loan.Length, ncol=1) +
  scale_color_gradient(low = "red", high = "green")

names(loansData)
summary(loansData)

# interest rate and FICO score lm
lm1 <- lm( loansData$Interest.Rate ~ FICO.Range, data=loansData)

summary(lm1)


# step?
lmS <- lm(Interest.Rate ~ ., data=loansData)
summary(lmS)

step(lmS)


# How does it perform?
lf1 <- fortify(lm1)

ggplot(lf1, aes(.fitted, .resid)) + 
  geom_hline(yintercept=0) + 
  geom_point() + 
  geom_smooth(se=F)

ggplot(lf1, aes(FICO.Range, .resid)) + 
  geom_hline(yintercept=0, color='red') + 
  geom_boxplot() + 
  theme(axis.text.x=element_text(angle = 90, hjust = 1))

# what if we add other perdictors?
lm2 <- lm( loansData$Interest.Rate ~ FICO.Range + Debt.To.Income.Ratio + Loan.Length, data=loansData)
lm3 <- lm( loansData$Interest.Rate ~ FICO.Range + Debt.To.Income.Ratio, data=loansData)

anova(lm1, lm2, lm3, lmAll, test = "F")

lmAll <- lm( loansData$Interest.Rate ~ FICO.Range + Debt.To.Income.Ratio + Loan.Length + Loan.Purpose + Monthly.Income, 
             data=loansData)
anova(lmAll)

confint(lm1)
confint(lm2)

plot(lm1)
plot(lm2)

# how does this perform?
lf2 <- fortify(lm2)

ggplot(lf2, aes(.fitted, .resid)) + 
  geom_hline(yintercept=0, color='red') + 
  geom_point() + 
  geom_smooth(se=F)

ggplot(data=lf2, aes(x=FICO.Range, y=.resid)) + 
  geom_hline(yintercept=0, color='red') + 
  geom_boxplot() +
  theme(axis.text.x=element_text(angle = 90, hjust = 1)) +
  facet_wrap(~ Loan.Length, ncol=1)

ggplot(data=lf1, aes(x=FICO.Range, y=.resid)) + 
  geom_hline(yintercept=0, color='red') + 
  geom_boxplot() +
  theme(axis.text.x=element_text(angle = 90, hjust = 1))

ggplot(data=lf2, aes(x=FICO.Range, y=.resid)) + 
  geom_hline(yintercept=0, color='red') + 
  geom_boxplot() +
  theme(axis.text.x=element_text(angle = 90, hjust = 1))







#
# Final plots for presentation
#----------------------------------------------------

# Initial Features
ggplot(data=loansData, aes(x=FICO.Range, y=Interest.Rate) ) + 
#  geom_boxplot() + 
  geom_point(position='jitter') + 
  labs(x='FICO Range', y='Interest Rate', title='Comparing Debt/Income Ratio with Interest Rate') +
  theme(axis.text.x=element_text(angle = 90, hjust = 1, vjust=0))

# Features used
breaks <- c(10, 20, 30)
labels <- loansData$FICO.Range[breaks]

p1 <- ggplot(data=loansData, aes(x=Debt.To.Income.Ratio, y=Interest.Rate, color=as.numeric(FICO.Range) ) ) + 
  geom_point(alpha=0.7) + 
  facet_wrap(~ Loan.Length, ncol=1) +
  scale_color_gradient(low = "red", high = "green", breaks=breaks, labels=labels) +
  labs(x='Debt to Income Ratio', y='Interest Rate', color='FICO Range', title='Selected Model Features') +
  theme(plot.title=element_text(size=10), 
        legend.title=element_text(size=8),
        legend.text=element_text(size=8),
        axis.text.x=element_text(size=6), axis.text.y=element_text(size=6),
        axis.title.x=element_text(size=8), axis.title.y=element_text(size=8) )

#
p2 <- ggplot(data=lf1, aes(x=FICO.Range, y=.resid)) + 
  geom_hline(yintercept=0, color='red') + 
  geom_boxplot() +
  theme(axis.text.x=element_text(angle = 90, hjust = 1, vjust=0, size=6)) +
  labs(x='FICO Range', y='Residual', color='FICO Range', title='Residuals: Model with FICO Score') +
  theme(plot.title=element_text(size=10), 
        legend.title=element_text(size=10),
        axis.text.x=element_text(size=6), axis.text.y=element_text(size=6),
        axis.title.x=element_text(size=8), axis.title.y=element_text(size=8) )

p3 <- ggplot(data=lf2, aes(x=FICO.Range, y=.resid)) + 
  geom_hline(yintercept=0, color='red') + 
  geom_boxplot() +
  theme(axis.text.x=element_text(angle = 90, hjust = 1, vjust=0, size=6)) +
  labs(x='FICO Range', y='Residual', color='FICO Range', title='Residuals: Full Model ') +
  theme(plot.title=element_text(size=10), 
        legend.title=element_text(size=10),
        legend.text=element_text(size=10),
        axis.text.x=element_text(size=6), axis.text.y=element_text(size=6),
        axis.title.x=element_text(size=8), axis.title.y=element_text(size=8) )


png(filename=file.path(analysisDir, 'foo.png', width=12, height=3, units='in', res=110, pointsize=6)

pushViewport(viewport(layout=grid.layout(nrow=1, ncol=3)))
print(p1, vp=vplayout(1, 1))
print(p2, vp=vplayout(1, 2))
print(p3, vp=vplayout(1, 3))

dev.off()
