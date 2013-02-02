#'
#' Quiz 2
#' 

twitterURL <- 'http://simplystatistics.tumblr.com/'

tData <- readLines(twitterURL, n=150)

head(tData)

nchar(tData[c(2, 45, 122)])

# data for questions 3 on
dataDir <- './data'
fileUrl <- 'https://spark-public.s3.amazonaws.com/dataanalysis/ss06hid.csv'
destFile <- file.path(dataDir, "ss06hid.csv")

download.file(fileUrl, destfile=destFile, method="curl")

hData <- read.csv(destFile)

str(hData)

# question 3
over1m <- subset(hData, VAL == 24)
nrow(over1m)

summary(hData$FES)

# BDS bedrooms
# RMS Rooms total

# Use the data you loaded from Question 3. 
# How many households have 3 bedrooms and and 4 total rooms? 
q5a <- subset(hData, BDS == 3 & RMS == 4)

# How many households have 2 bedrooms and 5 total rooms?
q5b <- subset(hData, BDS == 2 & RMS == 5)

# How many households have 2 bedrooms and 7 total rooms?
q5c <- subset(hData, BDS == 2 & RMS == 7)

nrow(q5a)
nrow(q5b)
nrow(q5c)

#' Question 6
#' Use the data from Question 3. 
#' Create a logical vector that identifies 
#' the households on greater than 10 acres who sold more than $10,000 worth of agriculture products. 
#' Assign that logical vector to the variable agricultureLogical. 
#' 
#' ACR == 3
#' AGS == 6
#' 
#' Apply the which() function like this to identify the rows of the data frame where the logical vector is TRUE. 
#'

agricultureLogical <- hData$ACR == 3 & hData$AGS == 6
length(agricultureLogical)
nrow(hData)

which(agricultureLogical) 

# QUestion 7

indexes <- which(agricultureLogical) 

subsetDataFrame <- hData[indexes, ]

nrow(subset(hData, ACR == 3 & AGS == 6))

length(which( is.na(subsetDataFrame$MRGX)))

# Question 8
hNames <- names(hData)
strsplit(hNames, 'wgtp')[123]

# Question 9 

dataDir <- './data'
fileUrl <- 'https://spark-public.s3.amazonaws.com/dataanalysis/ss06pid.csv'
destFile <- file.path(dataDir, "ss06pid.csv")

download.file(fileUrl, destfile=destFile, method="curl")

pData <- read.csv(destFile)
nrow(pData)
names(pData)
str(pData)

housingData <- hData
populationData <- pData

hpData <- merge(housingData, populationData, by='SERIALNO')
nrow(hpData)
ncol(hpData)

phData <- merge(populationData, housingData, by='SERIALNO')
nrow(phData)
ncol(phData)

mm <- merge(pData, hData, by='SERIALNO', all=TRUE)
nrow(mm)
ncol(mm)












