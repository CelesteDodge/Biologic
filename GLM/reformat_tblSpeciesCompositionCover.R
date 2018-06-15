#author: Celeste Dodge
#Date: June 11, 2018
#Project: GLM
#Purpose: Reformat raw species data into matrices for Weiss analysis
#Requirements: libraries reshape and dplyr

#clear workspace, set working dir
rm(list= ls())
setwd("S:/Pepperwood/RESEARCH/1 PWD On-Site Research/GLM Grassland Monitoring Project/src/access")
#read in file
original_sp <- read.csv("tblSpeciesCompositionCover_export_1803.csv")
#Create TransectYear column
original_sp$TransectYear <- paste(original_sp$TransectID, original_sp$SamplingYear)

#____Step 1___construct the table to be populated with transect-year averages
library(reshape2)
head(original_sp)
longform <-melt(original_sp, id.vars = c("ID", "TransectID", "SamplingYear", "SamplingDate", "QuadratDistance", "TransectYear"))
head(longform)
#Rename the 'value' field to Pct Cover
names(longform) <- c("ID", "TransectID", "SamplingYear", "SamplingDate", "QuadratDistance", "TransectYear", "SpeciesCode", "PctCover")


#____Step 1.1___Examine data
#what are the different years?
unique(longform$SamplingYear)
#What values are there for PctCover?
unique(longform$PctCover)

#confirm PctCover in 2011 - 2015 are only 0 and 999 values (PctCover data not collected those years)
first5 <- longform[ which(longform$SamplingYear == "2011" | longform$SamplingYear == "2012" |longform$SamplingYear == "2013" | longform$SamplingYear == "2014" | longform$SamplingYear == "2015"), ]
unique(first5$PctCover)

#Remove years before 2016 and view cover unique values for pctCover
longform <- longform[longform$SamplingYear >= "2016",]
unique(longform$PctCover)


#____Step 1.1___Prepare data for averaging
#Fix problematic code values remove 999 (no data collected)) and -1 (cover is less than 1%)
longform <- longform[!longform$PctCover == "999",]
longform$PctCover[longform$PctCover == "-1"] <-"0.5"
unique(longform$PctCover)

#Reformat PctCover from a character variable to a numeric one
str(longform)
longform$PctCover <- as.numeric(as.character(longform$PctCover))


#____Step 2___aggregate percent cover values into average for each transect-year and species combination, NOTE: Base R aggregate() would not work for me
#better with dplyr
library(dplyr)
avgSpeciesCover<- longform %>%
  group_by(TransectYear, SpeciesCode) %>% 
  summarise(PctCover = mean(PctCover))

head(avgSpeciesCover)
str(avgSpeciesCover)
unique(avgSpeciesCover$PctCover)


#____Step 3___reformat that new data (Convert back to wide form)
head(avgSpeciesCover)
wideform <- dcast(avgSpeciesCover, TransectYear ~ SpeciesCode, value.var = "PctCover")
head(wideform)

#write the file
setwd("S:/Pepperwood/RESEARCH/1 PWD On-Site Research/GLM Grassland Monitoring Project/src")
write.csv(wideform, file = "output_tbl.csv")




 
####################################
#____Extras not used___reformat aggregated data
#Create TransectYearSpecies field
longform$TransectYearSpecies <- paste(longform$TransectYear, longform$SpeciesCode)
#attempted aggregation
#NOTE: Base R aggregate() would not work for me
avgSpeciesCover <- aggregate(PctCover ~ TransectYearSpecies, longform, mean)
#pull a species column back out of TransectYearSpecies column (start with the 12th character and end on the 17th)
avgSpeciesCover$Species <-  substr(avgSpeciesCover$TransectYearSpecies, 12, 17)
avgSpeciesCover$TransectYear <-  substr(avgSpeciesCover$TransectYearSpecies, 1, 11)
head(avgSpeciesCover)
unique(avgSpeciesCover$PctCover)
