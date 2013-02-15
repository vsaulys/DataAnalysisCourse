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

# now excel sheets
require(xlsx)

fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.xlsx?accessType=DOWNLOAD"
destFile <- file.path(dataDir, "camera.xlsx")

download.file(fileUrl, destfile=destFile, method="curl")

cameraData <- read.xlsx2(destFile, sheetIndex=1)
head(cameraData)
str(cameraData)

#'
#' Lecture Part 2
#' 

require(RJSONIO)

fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.json?accessType=DOWNLOAD"
destFile <- file.path(dataDir, 'camera.json')

download.file(fileUrl, destfile=destFile, method="curl")

# WARNING, the file needs a return at the end, e.g. after the } in the downlaoded file

con = file(destFile)
jsonCamera = fromJSON(con)
close(con)
head(jsonCamera)


