#'
#' Data Analysis Assignment 2
#' 

require (plyr)
require (ggplot2)
require(grid)

#'
#' Function for helping layout grid graph elements
#' .x X location for plot
#' .y Y location for plot
#' 
vplayout <- function(.x, .y) {
  viewport(layout.pos.row=.x, layout.pos.col=.y)
}

# directory locations
analysisDir <- 'Analysis2'
dataDir <- file.path(analysisDir, 'data')

# read in the data
load(file.path(dataDir, 'samsungData.rda'))

names(samsungData)

summary(samsungData$activity)

# some data reworking
samsungData$activity <- factor(samsungData$activity)
summary(samsungData$activity)





