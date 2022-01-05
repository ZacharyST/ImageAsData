'''
This script identifies the most shared images, and these images are downloaded for Figure 7.
'''
###########
#
# GLOBALS
#
###########
rm(list=ls())

setwd('the/working/directory')

library(plyr)  # round_any.  has to go before dplyr for namespace issues
library(dplyr)
library(stargazer)
library(ggplot2)


makeShortData <- function(data){
  # Subset cities
  short <- data

  # Get rid of bad observations
  short <- short[short$country != 'EG',]  # Had wrong dates for Egypt
  short <- short[short$city_use != 'Guayabal',]  # Country level for Venezuela
  short <- short[short$city_use != 'Puerto Carreno',]  # Country level for Venezuela as well
  short <- short[short$city_use != 'Baykit',]  # Russia country  level
  short <- short[short$city_use != 'Pir Mahal',]  # Pakistan country level
  short <- short[short$city_use != 'Asilah',]  # A town in Morocco, not sure how it got here

  short <- short[short$country != 'RU',]  # Cross-section, not temporal
  short <- short[short$city_use != 'Smilavichy',]  # Centroid for Belarus, so country level.

  return(short)
}


###########
# LOAD, PROCESS THE DATA
###########
data <- read.csv('/the/original/data', stringsAsFactors=FALSE)  # These data are raw tweets and therefore cannot be shared.

data <- data[is.na(data$deduplicate_id) == FALSE,]  # Because of tweet rot, some images could not be downloaded and tested.  Therefore, should drop.

data$day <- substr(data$hour, 1, 10)
data$weekday <- weekdays(as.Date(data$day))

data <- makeShortData(data)

data$country <- data$place.country_code

data$country <- gsub("b'", "", data$country)
data$country <- gsub("'", "", data$country)

### Get table of number of times image id duplicated
## 1 = not duplicated
duplicates <- data.frame(table(data$deduplicate_id))
names(duplicates) <- c('deduplicate_id', 'Duplicate_Count')

## Merge back into main data
data <- merge(data, duplicates, by.x='deduplicate_id', by.y='deduplicate_id', all.x=TRUE, sort=FALSE)

data$isdup <- ifelse(data$Duplicate_Count > 1, 1, 0)


###########
# IDENTIFY THE MOST DUPLICATED PHOTOS
###########

### IDENTIFY SAMPLE IMAGES FOR PAPER
table(data$Duplicate_Count)  # 323, 52, 44, 37
popular1 <- subset(data, Duplicate_Count == 323)  # Venezuela, http://pbs.twimg.com/media/B67DzC6IQAAYEA7.jpg, group_20 .8498, protest .9570
popular2 <- subset(data, Duplicate_Count == 52)  # Venezuela, http://pbs.twimg.com/media/B7VaBv8IIAAJ5N6.jpg, group_20 .9010, protest = .9843
popular3 <- subset(data, Duplicate_Count == 44)  # Korea, http://pbs.twimg.com/media/CxwnpnaUAAA-05y.jpg, is of a sign
popular4 <- subset(data, Duplicate_Count == 37)  # Catalonia, http://pbs.twimg.com/media/DLCa7_CXUAA_RUy.jpg, state_violence is .4397

### I DOWNLOADED THESE IMAGES AND MANUALLY CREATED FIGURE 7.  AS OF 01.05.2021, THE LINKS ARE STILL ACTIVE.
