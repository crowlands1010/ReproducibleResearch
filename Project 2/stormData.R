library(ggplot2)
library(plyr)
library(knitr)
library(grid)

## Download and extract zip file
if (!file.exists("data")) {
  dir.create("data")
}

fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2" ##testdata
download.file(fileUrl,destfile="./data/repdata_data_StormData.csv.bz2")

## Loading and preprocessing the data
## Load the data (i.e. `read.csv()`)
fStorm <- read.csv(bzfile("data/repdata_data_StormData.csv.bz2"))
fStormData <- fStorm[, c("EVTYPE","BGN_DATE","FATALITIES","INJURIES","PROPDMG",
                         "PROPDMGEXP","CROPDMG","CROPDMGEXP")]
