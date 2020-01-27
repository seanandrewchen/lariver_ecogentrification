# Set Up ---------------------------------------------------------------------------------------------------------------------------------------------------------------

#Set working directory
setwd("~/Dropbox/academics/graduate/wws-mpa2/semester_02/final-projects/wws538-research/LA_River_GeospatialAnalysis/LARiver-StudyArea-Parcels")

#Load libraries
library(rgdal)
library(rgeos)
library(raster)
library(dplyr)
library(readr)



# Get Data for Project Area Parcels ---------------------------------------------------------------------------------------------------------------------------------------------------------------

#Get list of project area parcel AINs by reading in shapefile, creating dataframe, and exporting CSV
parcels.shape <- shapefile("StudyArea-Parcels-Residential-RiverDistance-Blocks-StationDistance-ParkDistance-SchoolDistrict-SchoolDistance-LandUseDistance.shp")

parcels.data <- as.data.frame(parcels.shape@data)

write_csv(parcels.data, "projectarea_parcels_withdistances.csv")

parcels.shape <- readOGR(dsn=".", layer="StudyArea-Parcels-Residential-RiverDistance-Blocks-StationDistance-ParkDistance-SchoolDistrict-SchoolDistance-LandUseDistance.shp")
parcels.df <- as(parcels.shape, "data.frame")
parcels.df <- subset(parcels.df, select = c("AIN", "Shape_Area"))

#Get list of parcels with distance attribute
parcels.distance <- readOGR(dsn=".", layer="Parcel2_11mileRiver_Distance")
parcels.distance.df <- as(parcels.distance, "data.frame")
parcels.distance.df <- subset(parcels.distance.df, select = c("AIN","HubDist") )

#Combine dataframes
parcels <- merge(parcels.df, parcels.distance.df, by="AIN")

#Write out new CSV
write_csv(parcels, "11mileriver_parcels_withdistance.csv")

#Parcels by River with Data
parcels.by.river <- read_csv("parcels_by_river.csv")

#Add shape area
parcels.by.river <- merge(parcels.df, parcels.by.river, by="AIN")

#Remove temporary vector
rm(parcels.df)
rm(parcels.shape)
rm(parcels.distance)
rm(parcels.distance.df)



# Clean Up Data ---------------------------------------------------------------------------------------------------------------------------------------------------------------

#Inspect data and look at column names
colnames(parcels.by.river)
sapply(parcels.by.river, class)

#Drop observations with wacky dates
parcels.by.river <- subset(parcels.by.river, YearBuilt > 1000)
parcels.by.river <- subset(parcels.by.river, EffectiveYearBuilt > 1000)

# Clean up the price variables stored as strings with dollar signs
price.variables <- vars(LandValue, ImprovementValue, TotalLandImpValue, HomeownersExemption,
                        RealEstateExemption, FixtureValue, FixtureExemption, PersonalPropertyValue,
                        PersonalPropertyExemption, TotalValue, TotalExemption, netTaxableValue)

parcels.by.river <- parcels.by.river %>%
  mutate_at(.vars = price.variables,
            .funs = funs(as.numeric(gsub("\\$", "", .)))) %>%
  as.data.frame()

#Drop observations with zero values
parcels.by.river <- subset(parcels.by.river, TotalValue > 0)
parcels.by.river <- subset(parcels.by.river, LandValue > 0)

#Drop Columns
columns.drop <- c("GeneralUseType", "PropertyType", "SpecificUseDetail1", "SpecificUseDetail2","HomeownersExemption", "RecordingDate", "TotalExemption", "netTaxableValue", "AssessorID", "LandBaseYear", "ImpBaseYear", "RealEstateExemption", "FixtureExemption", "PersonalPropertyExemption", "isTaxableParcel.")
parcels.by.river <- parcels.by.river[,!colnames(parcels.by.river) %in% columns.drop]

#Remove temporary vectors
rm(price.variables)
rm(columns.drop)



# Create dummies for land use types ---------------------------------------------------------------------------------------------------------------------------------------------------------------

#Get an idea of what the use codes are
property.use.codes.values <- read.csv("la_county_property_use_codes.csv")
property.use.codes.freqs <- as.data.frame(table(parcels.by.river$PropertyUseCode))
colnames(property.use.codes.freqs)
colnames(property.use.codes.values)
property.use.freqs <- merge(property.use.codes.values, property.use.codes.freqs, by.x = "use_code", by.y = "Var1")
rm(property.use.codes.freqs)
rm(property.use.codes.values)
rm(property.use.freqs)

#Create dummy variables
parcels.by.river$residential <- ifelse(grepl("^0", parcels.by.river$PropertyUseCode), 1, 0)
parcels.by.river$commercial <- ifelse(grepl("^1|^2|^6", parcels.by.river$PropertyUseCode), 1, 0)
parcels.by.river$industrial <- ifelse(grepl("^3|^4|^5", parcels.by.river$PropertyUseCode), 1, 0)
parcels.by.river$institutional <- ifelse(grepl("^7|^8", parcels.by.river$PropertyUseCode), 1, 0)
parcels.by.river$vacant <- ifelse(grepl("V$", parcels.by.river$PropertyUseCode), 1, 0)
parcels.by.river$parkinglot <- ifelse(grepl("^27|^38|^711", parcels.by.river$PropertyUseCode), 1, 0)
parcels.by.river$sfr <- ifelse(grepl("^01", parcels.by.river$PropertyUseCode), 1, 0)
parcels.by.river$pool <- ifelse(grepl("^0", parcels.by.river$PropertyUseCode) & grepl("1$", parcels.by.river$PropertyUseCode), 1, 0)
parcels.by.river$multires <- ifelse(grepl("^02|^03|^04|^05", parcels.by.river$PropertyUseCode), 1, 0)



# Create dataframes for each year ---------------------------------------------------------------------------------------------------------------------------------------------------------------

#Create dataframes for each year
parcels.2006 <- subset(parcels.by.river, RollYear == 2006)
parcels.2007 <- subset(parcels.by.river, RollYear == 2007)
parcels.2008 <- subset(parcels.by.river, RollYear == 2008)
parcels.2009 <- subset(parcels.by.river, RollYear == 2009)
parcels.2010 <- subset(parcels.by.river, RollYear == 2010)
parcels.2011 <- subset(parcels.by.river, RollYear == 2011)
parcels.2012 <- subset(parcels.by.river, RollYear == 2012)
parcels.2013 <- subset(parcels.by.river, RollYear == 2013)
parcels.2014 <- subset(parcels.by.river, RollYear == 2014)
parcels.2015 <- subset(parcels.by.river, RollYear == 2015)
parcels.2016 <- subset(parcels.by.river, RollYear == 2016)
parcels.2017 <- subset(parcels.by.river, RollYear == 2017)


# Create CSVs for each dataframe
write_csv(parcels.by.river, "parcels.csv")
write_csv(parcels.2006, "parcels2006.csv")
write_csv(parcels.2007, "parcels2007.csv")
write_csv(parcels.2008, "parcels2008.csv")
write_csv(parcels.2009, "parcels2009.csv")
write_csv(parcels.2010, "parcels2010.csv")
write_csv(parcels.2011, "parcels2011.csv")
write_csv(parcels.2012, "parcels2012.csv")
write_csv(parcels.2013, "parcels2013.csv")
write_csv(parcels.2014, "parcels2014.csv")
write_csv(parcels.2015, "parcels2015.csv")
write_csv(parcels.2016, "parcels2016.csv")
write_csv(parcels.2017, "parcels2017.csv")

#Create CSV with AIN, each year value, and each year change 



colnames(parcels.2006)[colnames(parcels.2006) == 'TotalValue'] <- 'TotalValue2006'
colnames(parcels.2007)[colnames(parcels.2007) == 'TotalValue'] <- 'TotalValue2007'
colnames(parcels.2008)[colnames(parcels.2008) == 'TotalValue'] <-'TotalValue2008'
colnames(parcels.2009)[colnames(parcels.2009) == 'TotalValue'] <-'TotalValue2009'
colnames(parcels.2010)[colnames(parcels.2010) == 'TotalValue'] <-'TotalValue2010'
colnames(parcels.2011)[colnames(parcels.2011) == 'TotalValue'] <-'TotalValue2011'
colnames(parcels.2012)[colnames(parcels.2012) == 'TotalValue'] <-'TotalValue2012'
colnames(parcels.2013)[colnames(parcels.2013) == 'TotalValue'] <-'TotalValue2013'
colnames(parcels.2014)[colnames(parcels.2014) == 'TotalValue'] <-'TotalValue2014'
colnames(parcels.2015)[colnames(parcels.2015) == 'TotalValue'] <-'TotalValue2015'
colnames(parcels.2016)[colnames(parcels.2016) == 'TotalValue'] <-'TotalValue2016'
colnames(parcels.2017)[colnames(parcels.2017) == 'TotalValue'] <-'TotalValue2017'

parcel2006.values <- subset(parcels.2006, select = c("AIN", "TotalValue2006"))
parcel2007.values <- subset(parcels.2007, select = c("AIN", "TotalValue2007"))
parcel2008.values <- subset(parcels.2008, select = c("AIN", "TotalValue2008"))
parcel2009.values <- subset(parcels.2009, select = c("AIN", "TotalValue2009"))
parcel2010.values <- subset(parcels.2010, select = c("AIN", "TotalValue2010"))
parcel2011.values <- subset(parcels.2011, select = c("AIN", "TotalValue2011"))
parcel2012.values <- subset(parcels.2012, select = c("AIN", "TotalValue2012"))
parcel2013.values <- subset(parcels.2013, select = c("AIN", "TotalValue2013"))
parcel2014.values <- subset(parcels.2014, select = c("AIN", "TotalValue2014"))
parcel2015.values <- subset(parcels.2015, select = c("AIN", "TotalValue2015"))
parcel2016.values <- subset(parcels.2016, select = c("AIN", "TotalValue2016"))
parcel2017.values <- subset(parcels.2017, select = c("AIN", "TotalValue2017"))




all.values <- list(parcel2006.values, parcel2007.values, parcel2008.values, parcel2009.values, parcel2010.values, parcel2011.values, parcel2012.values,
                   parcel2013.values, parcel2014.values, parcel2015.values, parcel2016.values, parcel2017.values)
parcel.values <- Reduce(function(x, y) merge(x, y, all=TRUE), 
                 all.values)
parcel.values <- na.exclude(parcel.values)
parcel.values$percent.2007 <- (parcel.values$TotalValue2007 - parcel.values$TotalValue2006) / parcel.values$TotalValue2006
parcel.values$percent.2008 <- (parcel.values$TotalValue2008 - parcel.values$TotalValue2006) / parcel.values$TotalValue2006
parcel.values$percent.2009 <- (parcel.values$TotalValue2009 - parcel.values$TotalValue2006) / parcel.values$TotalValue2006
parcel.values$percent.2010 <- (parcel.values$TotalValue2010 - parcel.values$TotalValue2006) / parcel.values$TotalValue2006
parcel.values$percent.2011 <- (parcel.values$TotalValue2011 - parcel.values$TotalValue2006) / parcel.values$TotalValue2006
parcel.values$percent.2012 <- (parcel.values$TotalValue2012 - parcel.values$TotalValue2006) / parcel.values$TotalValue2006
parcel.values$percent.2013 <- (parcel.values$TotalValue2013 - parcel.values$TotalValue2006) / parcel.values$TotalValue2006
parcel.values$percent.2014 <- (parcel.values$TotalValue2014 - parcel.values$TotalValue2006) / parcel.values$TotalValue2006
parcel.values$percent.2015 <- (parcel.values$TotalValue2015 - parcel.values$TotalValue2006) / parcel.values$TotalValue2006
parcel.values$percent.2016 <- (parcel.values$TotalValue2016 - parcel.values$TotalValue2006) / parcel.values$TotalValue2006
parcel.values$percent.2017 <- (parcel.values$TotalValue2017 - parcel.values$TotalValue2006) / parcel.values$TotalValue2006

write_csv(parcel.values, "parcelvalues.csv")


parcel.values.nooutliers <- Reduce(function(x, y) merge(x, y, all=TRUE), 
                        all.values)
parcel.values.nooutliers <- na.exclude(parcel.values.nooutliers)
parcel.values.nooutliers$percent.2007 <- (parcel.values.nooutliers$TotalValue2007 - parcel.values.nooutliers$TotalValue2006) / parcel.values.nooutliers$TotalValue2006
parcel.values.nooutliers$percent.2008 <- (parcel.values.nooutliers$TotalValue2008 - parcel.values.nooutliers$TotalValue2007) / parcel.values.nooutliers$TotalValue2007
parcel.values.nooutliers$percent.2009 <- (parcel.values.nooutliers$TotalValue2009 - parcel.values.nooutliers$TotalValue2008) / parcel.values.nooutliers$TotalValue2008
parcel.values.nooutliers$percent.2010 <- (parcel.values.nooutliers$TotalValue2010 - parcel.values.nooutliers$TotalValue2009) / parcel.values.nooutliers$TotalValue2009
parcel.values.nooutliers$percent.2011 <- (parcel.values.nooutliers$TotalValue2011 - parcel.values.nooutliers$TotalValue2010) / parcel.values.nooutliers$TotalValue2010
parcel.values.nooutliers$percent.2012 <- (parcel.values.nooutliers$TotalValue2012 - parcel.values.nooutliers$TotalValue2011) / parcel.values.nooutliers$TotalValue2011
parcel.values.nooutliers$percent.2013 <- (parcel.values.nooutliers$TotalValue2013 - parcel.values.nooutliers$TotalValue2012) / parcel.values.nooutliers$TotalValue2012
parcel.values.nooutliers$percent.2014 <- (parcel.values.nooutliers$TotalValue2014 - parcel.values.nooutliers$TotalValue2013) / parcel.values.nooutliers$TotalValue2013
parcel.values.nooutliers$percent.2015 <- (parcel.values.nooutliers$TotalValue2015 - parcel.values.nooutliers$TotalValue2014) / parcel.values.nooutliers$TotalValue2014
parcel.values.nooutliers$percent.2016 <- (parcel.values.nooutliers$TotalValue2016 - parcel.values.nooutliers$TotalValue2015) / parcel.values.nooutliers$TotalValue2015
parcel.values.nooutliers$percent.2017 <- (parcel.values.nooutliers$TotalValue2017 - parcel.values.nooutliers$TotalValue2016) / parcel.values.nooutliers$TotalValue2016


write_csv(parcel.values.nooutliers, "parcelvaluesyeartoyear.csv")


# Create shapefiles with different years of housing values -------------------------------------------------------------------------------------------------------------------------------------
parcels.shape <- readOGR(dsn=".",layer="ProjectArea2_Parcels")
parcels.shape <- merge(parcels.shape, parcel.values, by = "AIN")


# Create histograms and heat maps ---------------------------------------------------------------------------------------------------------------------------------------------------------------


#Average and median age over time


#Average and median housing value over time





# Regression Analysis  ---------------------------------------------------------------------------------------------------------------------------------------------------------------

#Run regression
la.river.housing.model <-  lm(y ~ x1 + x2 + x3,
                      data = na.omit(parcels.by.river))







