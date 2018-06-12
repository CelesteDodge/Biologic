#author: Celeste Dodge
#Date: May 23 2018
#Project: GLM
#Purpose: Reformat raw species data into matrices for Weiss analysis

#clear workspace, set working dir
rm(list= ls())
setwd("S:/Pepperwood/RESEARCH/1 PWD On-Site Research/GLM Grassland Monitoring Project/src/access")

#read in files
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
longform$TransectYearSpecies <- paste(longform$TransectYear, longform$SpeciesCode)
#What values are there for PctCover?
unique(longform$PctCover)
#what about in the different years?
unique(longform$SamplingYear)
#confirm there are only 0 and 999 values for species cover in 2011 - 2015 and 2016 is the first year with cover data for individual species
year2011 <- longform[longform$SamplingYear == "2011",]
unique(year2011$PctCover)
year2015 <- longform[longform$SamplingYear == "2015",]
unique(year2015$PctCover)
year2016 <- longform[longform$SamplingYear == "2016",]
unique(year2016$PctCover)
year2017 <- longform[longform$SamplingYear == "2017",]
unique(year2017$PctCover)

#Remove years before 2016 and remove the -1 and 999 values (NEED TO GET CODE DEFINITIONS)
longform <- longform[longform$SamplingYear >= "2016",]
unique(longform$PctCover)
longform <- longform[!longform$PctCover == "999",]
longform <- longform[!longform$PctCover == "-1",]
unique(longform$PctCover)

#____Step 2___aggregate percent cover values into average by transect-year and species
###___WARNING THIS IS WHERE IT GOES WRONG PROBLEM = the data are bins. We need to find the actual values for percent cover of species
avgSpeciesCover <- aggregate(PctCover ~ TransectYearSpecies, longform, mean)
head(avgSpeciesCover)

#____Step 3___reformat that new data
#pull a species column back out of TransectYearSpecies column (start with the 12th character and end on the 17th)
avgSpeciesCover$Species <-  substr(avgSpeciesCover$TransectYearSpecies, 12, 17)
avgSpeciesCover$TransectYear <-  substr(avgSpeciesCover$TransectYearSpecies, 1, 11)
#Convert back to wide form
#Results of this step  do not look right 
#Transect/year AvgSpCover has values as high as 35 (TRISUB NAT01 2017), 
#for the same transect/year/species the wideform has a 1 value 
wideform <- dcast(avgSpeciesCover, TransectYear ~ Species, value.var = "PctCover")
head(wideform)

#write the file
setwd("S:/Pepperwood/RESEARCH/1 PWD On-Site Research/GLM Grassland Monitoring Project/src")
write.csv(wideform, file = "Stu_SpeciesTransectYear_matrix_test2.csv")

 


