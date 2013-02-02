#'
#' Follows along with Getting Data Lectures
#' 

# create the directory to download to
dataDir <- './data'
dir.create(dataDir, showWarnings=FALSE, recursive=TRUE)

fileUrl <- 'https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD'
destFile <- file.path(dataDir, 'cameras.csv')

download.file(fileUrl, destfile=destFile, method='curl')

cameraData <- read.csv(destFile)
names(cameraData)
str(cameraData)
