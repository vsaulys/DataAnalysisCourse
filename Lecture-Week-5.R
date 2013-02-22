#'
#' Week 5 Lectures
#'
#'
#'

require(ggplot2)

# Lecture: ANOVA with multiple factors

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

# Lecture: Binary Outcomes
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

# Lecture: Couting Outcomes
download.file("https://dl.dropbox.com/u/7710864/data/gaData.rda", destfile="./data/gaData.rda", method='curl')
load("./data/gaData.rda")
gaData$julian <- julian(gaData$date)
head(gaData)
              
              
              
              



