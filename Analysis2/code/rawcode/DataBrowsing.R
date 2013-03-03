#'
#' Data Analysis Assignment 2
#' 

require (plyr)
require (ggplot2)
require(grid)

vplayout <- function(.x, .y) {
  viewport(layout.pos.row=.x, layout.pos.col=.y)
}

analysisDir <- 'Analysis2'
dataDir <- file.path(analysisDir, 'data')

# read in the data
load(file.path(dataDir, 'samsungData.rda'))

names(samsungData)

summary(samsungData$activity)

# some data reworking
samsungData$activity <- factor(samsungData$activity)






