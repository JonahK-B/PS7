rm(list=ls())

library(readr)
library(stringr)
March2018 <- read_csv("~/GitHub/PS7/March2018.CSV")
detach("package:EBMAforecast", unload = TRUE)
detach("package:plyr", unload=TRUE)
library(dplyr)

head(March2018)


#2
crimeTypeByDay <- select(March2018, DateOccur, Description)## subsetting data set for day and type of crime
##I set up a new table for any modified information rather than directly changing the original so I have the original for reference in case I accidently change anything.
crimeTypeByDay <- as_tibble(crimeTypeByDay)

crimeTypeByDay$DateOccur <- substr(crimeTypeByDay$DateOccur, 1,10)  ##Isolating date, removing timestamp
crimeTypeByDay$Description <- strsplit(as.character(crimeTypeByDay$Description), "-") ## Generalizing types of crimes
crimeTypeByDay$Description <- sapply(crimeTypeByDay$Description, function(x) x[1])


##Counting number of different types of crimes by day
## (some of these crimes happened before March 2018, but they are all reported in March)
crimeFreq <- crimeTypeByDay %>%
group_by(DateOccur, Description) %>%
  dplyr::summarise(count = n()) ##Specifying dplyr because there is a conflict with plyr

##Counting most frequent March crimes
crimeFreq1 <- crimeTypeByDay %>%
  group_by(Description) %>%
  dplyr::summarise(count = n())

arrange(crimeFreq1, desc(count)) ## Larceny: 969, Leaving Scene of Accident: 464, Destruction of property: 295

#3
crimeTypeByNH <- select(March2018, Neighborhood, DateOccur)
crimeTypeByNH <- as_tibble(crimeTypeByNH)

crimeTypeByNH$DateOccur <- substr(crimeTypeByNH$DateOccur, 1,10)  ##Isolating date, removing timestamp



##Arranging by date and neighborhood
crimeNHFreq <- crimeTypeByNH %>%
  group_by(DateOccur, Neighborhood) %>%
  dplyr::summarise(count = n()) ##Specifying dplyr because there is a conflict with plyr


##Counting crimes by neighborhood alone
crimeNHFreq1 <- crimeTypeByNH %>%
  group_by(Neighborhood) %>%
  dplyr::summarise(count = n())
arrange(crimeNHFreq1, desc(count)) ## NH 35: 305 crimes, NH 36: 203 crimes, NH 16: 185 crimes


#4
districtCrime <- select(March2018, District, Description)
districtCrime <- as_tibble(districtCrime)


districtRobbery <- districtCrime %>%
filter(str_detect(Description, "ROBBERY")) %>%
  group_by(District) %>%
  summarise(countRobbery = n())

districtTotalCrime <- districtCrime %>%
  group_by(District) %>%
  summarise(countTotalCrime = n())

robberyProportion <- right_join(districtRobbery, districtTotalCrime)
robberyProportion[1,2] <- 0
robberyProportion$robProp <- robberyProportion$countRobbery/robberyProportion$countTotalCrime

arrange(robberyProportion, desc(robProp)) ## District 5 has the highest percentage of robberies, at ~4%


#5
library(ggplot2)

crimeTypeByDay <- select(March2018, DateOccur, Description)## subsetting data set for day and type of crime
##I set up a new table for any modified information rather than directly changing the original so I have the original for reference in case I accidently change anything.
crimeTypeByDay <- as_tibble(crimeTypeByDay)

crimeTypeByDay$DateOccur <- substr(crimeTypeByDay$DateOccur, 1,10)  ##Isolating date, removing timestamp
crimeTypeByDay$Description <- strsplit(as.character(crimeTypeByDay$Description), "-") ## Generalizing types of crimes
crimeTypeByDay$Description <- sapply(crimeTypeByDay$Description, function(x) x[1])


##Counting number of different types of crimes by day
## (some of these crimes happened before March 2018, but they are all reported in March)
crimeFreq <- crimeTypeByDay %>%
  group_by(Description, DateOccur) %>%
  dplyr::summarise(count = n()) ##Specifying dplyr because there is a conflict with plyr




  
