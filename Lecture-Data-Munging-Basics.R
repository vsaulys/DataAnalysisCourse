#'
#' Lecture: Data Munging Basics
#' 

#'
#' Lecture: Summarizing Data
#' 
#' 

# create the directory to download to
dataDir <- './data'
dir.create(dataDir, showWarnings=FALSE, recursive=TRUE)

cameraData <- read.csv(file.path(dataDir, 'cameras.csv'))

names(cameraData)

splitNames <- strsplit(names(cameraData), '\\.')
splitNames[[5]]

splitNames[[6]]

firstElement <- function(x) {
  return( x[1] )
}

sapply(splitNames, firstElement)

names(cameraData) <- tolower(sapply(splitNames, firstElement))
names(cameraData)

# reviews data
# Peer review data
fileUrl1 <- "https://dl.dropbox.com/u/7710864/data/reviews-apr29.csv"
fileUrl2 <- "https://dl.dropbox.com/u/7710864/data/solutions-apr29.csv"

destFile1 <- file.path(dataDir, 'reviews.csv')
destFile2 <- file.path(dataDir, 'solutions.csv')

download.file(fileUrl1, destfile=destFile1, method="curl")
download.file(fileUrl2, destfile=destFile2, method="curl")

reviews <- read.csv(destFile1)
solutions <- read.csv(destFile2)

head(reviews,2)
head(solutions,2)

names(reviews)

names(reviews) <- gsub("_", ".", names(reviews))
names(reviews)

names(solutions) <- gsub("_", ".", names(solutions))
names(solutions)

summary(reviews$time.left)
timeRanges <- cut(reviews$time.left, seq(0, 3600, by=600))
timeRanges[1:10]

table(timeRanges, useNA='ifany')

library(Hmisc)

timeRanges <- cut2(reviews$time.left, g=6)
table(timeRanges, useNA='ifany')

reviews$timeRnages <- timeRanges
head(reviews, 2)

names(reviews)
names(solutions)

mergedData <- merge(reviews, solutions, all=TRUE)
head(mergedData)

mergedData2 <- merge(reviews, solutions, by.x='solution.id', by.y='id', all=TRUE)
head(mergedData2)

reviews[1, 1:6]

sort(mergedData2$reviewer.id)[1:10]

mergedData2$reviewer.id[1:10]
order(mergedData2$reviewer.id)[1:10]

mergedData2$reviewer.id[order(mergedData2$reviewer.id)]

head(mergedData2[,1:6], 3)

sortedData <- mergedData2[order(mergedData2$reviewer.id), ]
head(sortedData[,1:6],)

misShaped <- as.data.frame(matrix(c(NA, 5, 1, 4, 2, 3), byrow=TRUE, nrow=3))
names(misShaped) <- c('treamentA', 'treatmentB')
misShaped$people <- c('John', 'Jane', 'Mary')

require(reshape2)

melt(misShaped, id.vars='people', variable.name='treatment', value.name='value')







