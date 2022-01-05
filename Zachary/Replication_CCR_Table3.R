'''
This script generates Table 3, the frame alignment by frame and protest wave.
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
# SUBSETS BY COUNTRY
###########
##  SUBSETS
ve <- data[data$cc=='VE',]
ve <- ve[ve$day >= '2014-03-27' & ve$day <= '2015-02-10',]

# Look only at Saturdays, starting october 22
kr <- data[data$cc=='KR' & data$weekday == 'Saturday',]

hk <- data[data$cc == 'HK',]
hk <- hk[hk$day >= '2014-09-20' & hk$day <= '2014-12-15',]


# Spain
es <- data[data$cc == 'ES',]
es <- es[es$day >= '2017-09-01' & es$day <= '2017-12-31',]

# Russia
ru <- data[data$cc == 'RU',]
ru <- ru[ru$day == '2017-03-26',]

new <- rbind(ve, kr, hk, es, ru)

new$cc[new$cc=='VE'] <- 'Venezuela'
new$cc[new$cc=='KR'] <- 'South Korea'
new$cc[new$cc=='HK'] <- 'Hong Kong'
new$cc[new$cc=='ES'] <- 'Catalonia, Spain'
new$cc[new$cc=='RU'] <- 'Russia'


###############
# GET DUPLICATION RATE BY FRAME AND COUNTRY
###############
## STATE VIOLENCE
new$state_violence_rounded <- round(new$protest_result.state_violence, 3)
new$state_violence_rounded_40 <- round_any(new$protest_result.state_violence, accuracy=.025)
sv <- new %>% group_by(cc, state_violence_rounded_40) %>% summarize(rate=1-length(unique(deduplicate_id))/n(), images=n())
names(sv)[names(sv) == 'state_violence_rounded_40'] <- 'thevar'
sv$varname <- 'Violence, State'

## PROTESTER VIOLENCE
new$protester_violence_rounded <- round(new$protest_result.protester_violence, 3)
new$protester_violence_rounded_40 <- round_any(new$protest_result.protester_violence, accuracy=.025)
pv <- new %>% group_by(cc, protester_violence_rounded_40) %>% summarize(rate=1-length(unique(deduplicate_id))/n(), images=n())
names(pv)[names(pv) == 'protester_violence_rounded_40'] <- 'thevar'
pv$varname <- 'Violence, Protester'

## DUPLICATE RATE BY FACES IN PHOTO
faces <- new %>% group_by(cc, faces) %>% summarize(rate=1-length(unique(deduplicate_id))/n(), images=n())
names(faces)[names(faces) == 'faces'] <- 'thevar'
faces$varname <- 'Faces, Perc. of Max'

# Normalize faces by max for each country.
faces <- faces %>% group_by(cc) %>% mutate(thevar = thevar/max(thevar))

## DUPLICATE RATE BY SMALL GROUP
new$group_20_rounded <- round(new$protest_result.group_20, 3)
new$group_20_rounded_40 <- round_any(new$protest_result.group_20, accuracy=.025)
group20 <- new %>% group_by(cc, group_20_rounded_40) %>% summarize(rate=1-length(unique(deduplicate_id))/n(), images=n())
names(group20)[names(group20) == 'group_20_rounded_40'] <- 'thevar'
group20$varname <- 'Small Group'

## DUPLICATE RATE BY LARGE GROUP
new$group_100_rounded <- round(new$protest_result.group_100, 3)
new$group_100_rounded_40 <- round_any(new$protest_result.group_100, accuracy=.025)
group100 <- new %>% group_by(cc, group_100_rounded_40) %>% summarize(rate=1-length(unique(deduplicate_id))/n(), images=n())
names(group100)[names(group100) == 'group_100_rounded_40'] <- 'thevar'
group100$varname <- 'Large Group'

## DUPLICATE RATE BY CHILDREN
new$children_rounded <- round(new$protest_result.children, 3)
new$children_rounded_40 <- round_any(new$protest_result.children, accuracy=.025)
children <- new %>% group_by(cc, children_rounded_40) %>% summarize(rate=1-length(unique(deduplicate_id))/n(), images=n())
names(children)[names(children) == 'children_rounded_40'] <- 'thevar'
children$varname <- 'Children'


###############
# COMBINE INTO ONE DATASET
###############
use <- rbind(sv, pv, faces, group20, group100, children)

write.csv(use, 'Data/framealignment.csv', row.names=FALSE)

###############
# MAKE TABLE 3
###############
mu <- use %>% group_by(cc, varname) %>% summarize(grp.mean=mean(rate))  # This is Table 3


###############
# START HERE TO USE THE PROVIDED DATA
###############
loaded <- read.csv('framealignment.csv', stringsAsFactors = FALSE)
mu2 <- loaded %>% group_by(cc, varname) %>% summarize(grp.mean=mean(rate))



