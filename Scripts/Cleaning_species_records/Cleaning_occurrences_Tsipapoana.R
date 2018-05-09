#Script 1
#Cleaning database occurrences (from clean database)

#Remove points outside geographic area
#Reduce resolution of points (reduce sampling bias)
#Check for outliers?
#Save cleaned data as csv files
#Will need to modify this script for each species to model

#Load libraries
library(dplyr)
library(rgdal)
library(raster)
library(scrubr)
library(spatstat)

#Import points from file

Tsipapoana <- read.csv("./Data/Occurrence_records_by_species/Tsipapoana_unique.csv", stringsAsFactors = FALSE)
Tsipapoana <- Tsipapoana[,-c(2:5)]

colnames(Tsipapoana)[1] <- "name"

head(Tsipapoana)

Tsipapoana_clean <- coord_impossible(Tsipapoana)
nrow(Tsipapoana_clean)
#Remove points with no environmental data

#Make data spatial
Tsipapoana_spatial <- Tsipapoana_clean
coordinates(Tsipapoana_spatial) <- c("longitude", "latitude")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
proj4string(Tsipapoana_spatial) <- crs.geo


#Import environmental data
terrestrialAltitude <- raster("./Layers/terrestrialAltitude.asc")
south_america <- readOGR("./Layers/joined.shp")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
crs(terrestrialAltitude) <- crs.geo

crs(south_america)
crs(terrestrialAltitude) 
crs(Tsipapoana_spatial)

#Crop terrestrialAltitude to geographic area (South America)
crop_terrAlt <- crop(terrestrialAltitude, extent(south_america))

#Check that extents match
extent(south_america)
extent(crop_terrAlt)

#Extract points only from South America
Tsipapoana_extract <- extract(crop_terrAlt, Tsipapoana_spatial)

#Link complete dataset with extracted point yes/no
Tsipapoana_extract<- cbind(Tsipapoana_clean, Tsipapoana_extract)
Tsipapoana_extract

#Remove incomplete cases (those not in extracted dataset)
clean_Tsipapoana <- Tsipapoana_extract[complete.cases(Tsipapoana_extract[,4]),]

#Get resolution of raster of environmental data
rasterResolution <- max(res(terrestrialAltitude))
rasterResolution

#Reduce resolution
while(min(nndist(clean_Tsipapoana[,2:3])) < rasterResolution){
  nnD <- nndist(clean_Tsipapoana[,2:3])
  clean_Tsipapoana <- clean_Tsipapoana[-(which(min(nnD) == nnD)[1]),]
}

clean_Tsipapoana

#Set rownames of clean dataset
row.names(clean_Tsipapoana) <- seq(nrow(clean_Tsipapoana))


#Check for outliers by plotting
plot_Tsipapoana <- clean_Tsipapoana
coordinates(plot_Tsipapoana) <- c("longitude", "latitude")
proj4string(plot_Tsipapoana) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

plot(south_america)
plot(plot_Tsipapoana, add=TRUE, col = "purple")

#Save the clean data
write.csv(clean_Tsipapoana, "./Data/Cleaned_occurrences_by_species/Tsipapoana_cleaned_occurrences.csv", row.names = F)
