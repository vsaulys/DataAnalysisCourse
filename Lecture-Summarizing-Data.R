#'
#' Lecture: Summarizing Data
#' 
#' 

# create the directory to download to
dataDir <- './data'
dir.create(dataDir, showWarnings=FALSE, recursive=TRUE)

fileUrl <- "http://earthquake.usgs.gov/earthquakes/catalogs/eqs7day-M1.txt"
destFile <- file.path(dataDir, 'earthquakeData.csv')

download.file(fileUrl, destfile=destFile, method='curl')

dateDownloaded <- date()

dateDownloaded

eData <- read.csv(destFile)

head(eData)
dim(eData)

names(eData)
nrow(eData)

quantile(eData$Lat)
summary(eData)

class(eData)
sapply(eData[1,], class)

unique(eData$Src)

length(unique(eData$Src))

table(eData$Src)
table(eData$Src, eData$Version)

eData$Lat[1:10]

eData$Lat[1:10] > 40

any(eData$Lat[1:10] > 40)
all(eData$Lat[1:10] > 40)

eData[eData$Lat > 0 & eData$Lon > 0, c('Lat', 'Lon')]

# use plyr
subset(eData, Lat > 0 & Lon > 0)[, c('Lat', 'Lon')]

eData[eData$Lat < 0 | eData$Lon < 0, c('Lat', 'Lon')]

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
head(solutions, 2)

is.na(reviews$time_left[1:10])
sum(is.na(reviews$time_left))

table(is.na(reviews$time_left))

table(c(0,1,2,3,NA,3,3,2,2,3), useNA='ifany')

colSums(reviews)

colMeans(reviews, na.rm=TRUE)
rowMeans(reviews, na.rm=TRUE)



