# Set Up ---------------------------------------------------------------------------------------------------------------------------------------------------------------

#Set working directory
setwd("~/Dropbox/academics/graduate/wws-mpa2/semester_02/final-projects/wws538-research/LA_River_GeospatialAnalysis")

#Load libraries
library(rgdal)
library(rgeos)
library(dplyr)
library(readr)
library(raster)


# Get FIPS codes for Census geographies ---------------------------------------------------------------------------------------------------------------------------------
blockgroups.shape <- shapefile(x = "LARiver-StudyArea/LARiver_StudyArea_BlockGroups.shp")
blockgroups.df <- as.data.frame(blockgroups.shape)
keeps <- c("GEOID")
blockgroups.df <- blockgroups.df[keeps]

# Get ACS ---------------------------------------------------------------------------------------------------------------------------------------------------------------


#TOTAL POPULATION
acs16_b01003 <- read_csv("LARiver-StudyArea-Demographics/acs5yr2016/ACS_16_5YR_B01003_with_ann.csv")
keeps <- c("GEO.id2", "HD01_VD01")
acs16_b01003 <- acs16_b01003[keeps]
names(acs16_b01003) <- c("GEOID", "POPULATION2016")
acs16_b01003 <- acs16_b01003[-1, ]
acs16_b01003$POPULATION2016 <- as.numeric(acs16_b01003$POPULATION2016)



#RACE
acs16_b02001 <- read_csv("LARiver-StudyArea-Demographics/acs5yr2016/ACS_16_5YR_B02001_with_ann.csv")
keeps <- c("GEO.id2", "HD01_VD01", "HD01_VD02")
acs16_b02001 <- acs16_b02001[keeps]
names(acs16_b02001) <- c("GEOID", "POPULATION2016", "POPULATION_WHITE2016")
acs16_b02001 <- acs16_b02001[-1, ]
acs16_b02001$POPULATION2016 <- as.numeric(acs16_b02001$POPULATION2016)
acs16_b02001$POPULATION_WHITE2016 <- as.numeric(acs16_b02001$POPULATION_WHITE2016)
acs16_b02001$PERCENT_MINORITY_16 <- ((acs16_b02001$POPULATION2016 - acs16_b02001$POPULATION_WHITE2016) / acs16_b02001$POPULATION2016) * 100
keeps <- c("GEOID", "PERCENT_MINORITY_16")
acs16_b02001 <- acs16_b02001[keeps]


#HOUSING UNITS
acs16_b25001 <- read_csv("LARiver-StudyArea-Demographics/acs5yr2016/ACS_16_5YR_B25001_with_ann.csv")
keeps <- c("GEO.id2", "HD01_VD01")
acs16_b25001 <- acs16_b25001[keeps]
names(acs16_b25001) <- c("GEOID", "HOUSINGUNITS2016")
acs16_b25001 <- acs16_b25001[-1, ]
acs16_b25001$HOUSINGUNITS2016 <- as.numeric(acs16_b25001$HOUSINGUNITS2016)

#TENURE
acs16_b25003 <- read_csv("LARiver-StudyArea-Demographics/acs5yr2016/ACS_16_5YR_B25003_with_ann.csv")
keeps <- c("GEO.id2", "HD01_VD01", "HD01_VD02", "HD01_VD03")
acs16_b25003 <- acs16_b25003[keeps]
names(acs16_b25003) <- c("GEOID", "HOUSINGUNITS2016", "OWNEROCCUPIED2016", "RENTEROCCUPIED2016")
acs16_b25003 <- acs16_b25003[-1, ]
acs16_b25003$HOUSINGUNITS2016 <- as.numeric(acs16_b25003$HOUSINGUNITS2016)
acs16_b25003$OWNEROCCUPIED2016 <- as.numeric(acs16_b25003$OWNEROCCUPIED2016)
acs16_b25003$RENTEROCCUPIED2016 <- as.numeric(acs16_b25003$RENTEROCCUPIED2016)
acs16_b25003$PERCENT_OWNER_16 <- (acs16_b25003$OWNEROCCUPIED2016 / acs16_b25003$HOUSINGUNITS2016) * 100
acs16_b25003$PERCENT_RENTER_16 <- (acs16_b25003$RENTEROCCUPIED2016 / acs16_b25003$HOUSINGUNITS2016) * 100
keeps <- c("GEOID", "PERCENT_OWNER_16", "PERCENT_RENTER_16")
acs16_b25003 <- acs16_b25003[keeps]


#VACANCY
acs16_b25004 <- read_csv("LARiver-StudyArea-Demographics/acs5yr2016/ACS_16_5YR_B25004_with_ann.csv")
keeps <- c("GEO.id2", "HD01_VD02", "HD01_VD04")
acs16_b25004 <- acs16_b25004[keeps]
names(acs16_b25004) <- c("GEOID", "FORRENT2016", "FORSALE2016")
acs16_b25004 <- acs16_b25004[-1, ]
acs16_b25004$HOUSINGUNITS2016 <- as.numeric(acs16_b25001$HOUSINGUNITS2016)
acs16_b25004$FORRENT2016 <- as.numeric(acs16_b25004$FORRENT2016)
acs16_b25004$FORSALE2016 <- as.numeric(acs16_b25004$FORSALE2016)
acs16_b25004$PERCENT_FORRENT_16 <- (acs16_b25004$FORRENT2016 / acs16_b25004$HOUSINGUNITS2016) * 100
acs16_b25004$PERCENT_FORSALE_16 <- (acs16_b25004$FORSALE2016 / acs16_b25004$HOUSINGUNITS2016) * 100 
keeps <- c("GEOID", "PERCENT_FORRENT_16", "PERCENT_FORSALE_16")
acs16_b25004 <- acs16_b25004[keeps]



# Get Census 2010 ---------------------------------------------------------------------------------------------------------------------------------------------------------------



#POPULATION
dec_10_sf1_p1 <- read_csv("LARiver-StudyArea-Demographics/census2010/DEC_10_SF1_P1_with_ann.csv")
keeps <- c("GEO.id2", "D001")
dec_10_sf1_p1 <- dec_10_sf1_p1[keeps]
names(dec_10_sf1_p1) <- c("GEOID", "POPULATION2010")
dec_10_sf1_p1 <- dec_10_sf1_p1[-1, ]
dec_10_sf1_p1$POPULATION2010 <- as.numeric(dec_10_sf1_p1$POPULATION2010)


#RACE
dec_10_sf1_p3 <- read_csv("LARiver-StudyArea-Demographics/census2010/DEC_10_SF1_P3_with_ann.csv")
keeps <- c("GEO.id2", "D001", "D002")
dec_10_sf1_p3 <- dec_10_sf1_p3[keeps]
names(dec_10_sf1_p3) <- c("GEOID", "POPULATION2010", "POPULATION_WHITE2010")
dec_10_sf1_p3 <- dec_10_sf1_p3[-1, ]
dec_10_sf1_p3$POPULATION2010 <- as.numeric(dec_10_sf1_p3$POPULATION2010)
dec_10_sf1_p3$POPULATION_WHITE2010 <- as.numeric(dec_10_sf1_p3$POPULATION_WHITE2010)
dec_10_sf1_p3$PERCENT_MINORITY_10 <- ((dec_10_sf1_p3$POPULATION2010 - dec_10_sf1_p3$POPULATION_WHITE2010) / dec_10_sf1_p3$POPULATION2010) * 100
keeps <- c("GEOID", "PERCENT_MINORITY_10")
dec_10_sf1_p3 <- dec_10_sf1_p3[keeps]




#HOUSING
dec_10_sf1_h1 <- read_csv("LARiver-StudyArea-Demographics/census2010/DEC_10_SF1_H1_with_ann.csv")
keeps <- c("GEO.id2", "D001")
dec_10_sf1_h1 <- dec_10_sf1_h1[keeps]
names(dec_10_sf1_h1) <- c("GEOID", "HOUSINGUNITS2010")
dec_10_sf1_h1 <- dec_10_sf1_h1[-1, ]
dec_10_sf1_h1$HOUSINGUNITS2010 <- as.numeric(dec_10_sf1_h1$HOUSINGUNITS2010)


#TENURE
dec_10_sf1_h4 <- read_csv("LARiver-StudyArea-Demographics/census2010/DEC_10_SF1_H4_with_ann.csv")
keeps <- c("GEO.id2", "D001", "D004")
dec_10_sf1_h4 <- dec_10_sf1_h4[keeps]
names(dec_10_sf1_h4) <- c("GEOID", "HOUSINGUNITS2010", "RENTEROCCUPIED2010")
dec_10_sf1_h4 <- dec_10_sf1_h4[-1, ]
dec_10_sf1_h4$HOUSINGUNITS2010 <- as.numeric(dec_10_sf1_h4$HOUSINGUNITS2010)
dec_10_sf1_h4$RENTEROCCUPIED2010 <- as.numeric(dec_10_sf1_h4$RENTEROCCUPIED2010)
dec_10_sf1_h4$OWNEROCCUPIED2010 <- dec_10_sf1_h4$HOUSINGUNITS2010 - dec_10_sf1_h4$RENTEROCCUPIED2010
dec_10_sf1_h4$PERCENT_RENTER_2010 <- (dec_10_sf1_h4$RENTEROCCUPIED2010 / dec_10_sf1_h4$HOUSINGUNITS2010) * 100
dec_10_sf1_h4$PERCENT_OWNER_2010 <- (dec_10_sf1_h4$OWNEROCCUPIED2010 / dec_10_sf1_h4$HOUSINGUNITS2010) * 100
keeps <- c("GEOID", "PERCENT_RENTER_2010", "PERCENT_OWNER_2010")
dec_10_sf1_h4 <- dec_10_sf1_h4[keeps]

#VACANCY
dec_10_sf1_h5 <- read_csv("LARiver-StudyArea-Demographics/census2010/DEC_10_SF1_H5_with_ann.csv")
keeps <- c("GEO.id2", "D002", "D004")
dec_10_sf1_h5 <- dec_10_sf1_h5[keeps]
names(dec_10_sf1_h5) <- c("GEOID", "FORRENT2010", "FORSALE2010")
dec_10_sf1_h5 <- dec_10_sf1_h5[-1, ]
dec_10_sf1_h5$HOUSINGUNITS2010 <- as.numeric(dec_10_sf1_h1$HOUSINGUNITS2010)
dec_10_sf1_h5$FORRENT2010 <- as.numeric(dec_10_sf1_h5$FORRENT2010)
dec_10_sf1_h5$FORSALE2010 <- as.numeric(dec_10_sf1_h5$FORSALE2010)
dec_10_sf1_h5$PERCENT_FORRENT_2010 <- (dec_10_sf1_h5$FORRENT2010 / dec_10_sf1_h5$HOUSINGUNITS2010) * 100
dec_10_sf1_h5$PERCENT_FORSALE_2010 <- (dec_10_sf1_h5$FORSALE2010 / dec_10_sf1_h5$HOUSINGUNITS2010) * 100
keeps <- c("GEOID", "PERCENT_FORRENT_2010", "PERCENT_FORSALE_2010")
dec_10_sf1_h5 <- dec_10_sf1_h5[keeps]


frames <- list(blockgroups.df, acs16_b01003, acs16_b02001, acs16_b25001, acs16_b25003, acs16_b25004, dec_10_sf1_h1, dec_10_sf1_h4, dec_10_sf1_h5, dec_10_sf1_p1, dec_10_sf1_p3)

blockgroups.demographics.df <- Reduce(function(...) merge(..., by = "GEOID"), frames) 





blockgroups.demographics.df$POPULATION_CHANGE <- ((blockgroups.demographics.df$POPULATION2016 - blockgroups.demographics.df$POPULATION2010) / blockgroups.demographics.df$POPULATION2010) * 100
blockgroups.demographics.df$MINORITY_CHANGE <- blockgroups.demographics.df$PERCENT_MINORITY_16 - blockgroups.demographics.df$PERCENT_MINORITY_10
blockgroups.demographics.df$HOUSING_CHANGE <- ((as.numeric(blockgroups.demographics.df$HOUSINGUNITS2016) - as.numeric(blockgroups.demographics.df$HOUSINGUNITS2010)) / as.numeric(blockgroups.demographics.df$HOUSINGUNITS2010)) * 100
blockgroups.demographics.df$RENTER_CHANGE <- blockgroups.demographics.df$PERCENT_RENTER_16 - blockgroups.demographics.df$PERCENT_RENTER_2010
blockgroups.demographics.df$OWNER_CHANGE <- blockgroups.demographics.df$PERCENT_OWNER_16 - blockgroups.demographics.df$PERCENT_OWNER_2010
blockgroups.demographics.df$FORRENT_CHANGE <- blockgroups.demographics.df$PERCENT_FORRENT_16 - blockgroups.demographics.df$PERCENT_FORRENT_2010
blockgroups.demographics.df$FORSALE_CHANGE <- blockgroups.demographics.df$PERCENT_FORSALE_16 - blockgroups.demographics.df$PERCENT_FORSALE_2010

keeps <- c("GEOID", "POPULATION_CHANGE", "MINORITY_CHANGE", "HOUSING_CHANGE", "RENTER_CHANGE", "OWNER_CHANGE", "FORRENT_CHANGE", "FORSALE_CHANGE")
blockgroups.demographics.df <- blockgroups.demographics.df[keeps]
names(blockgroups.demographics.df) <- c("GEOID", "POP", "MIN", "HOUSE", "RENT", "OWN", "FORRENT", "FORSALE")




write_csv(blockgroups.demographics.df, "blockgroups_demographics.csv")


blockgroups.shape <- merge(blockgroups.shape, blockgroups.demographics.df, by.x = "GEOID", by.y = "GEOID")


outfile <- 'blockgroups-demographics.shp'
shapefile(blockgroups.shape, outfile, overwrite=TRUE)





