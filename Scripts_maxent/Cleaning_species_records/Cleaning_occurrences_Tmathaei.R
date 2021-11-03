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

Tmathaei <- read.csv("./Data/Occurrence_records_by_species/Tmathaei_unique.csv", stringsAsFactors = FALSE)
Tmathaei <- Tmathaei[,-c(2:5)]

colnames(Tmathaei)[1] <- "name"

head(Tmathaei)

Tmathaei_clean <- coord_impossible(Tmathaei)
nrow(Tmathaei_clean)
#Remove points with no environmental data

#Make data spatial
Tmathaei_spatial <- Tmathaei_clean
coordinates(Tmathaei_spatial) <- c("longitude", "latitude")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
proj4string(Tmathaei_spatial) <- crs.geo


#Import environmental data
terrestrialAltitude <- raster("./Layers/terrestrialAltitude.asc")
south_america <- readOGR("./Layers/joined.shp")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
crs(terrestrialAltitude) <- crs.geo

crs(south_america)
crs(terrestrialAltitude) 
crs(Tmathaei_spatial)

#Crop terrestrialAltitude to geographic area (South America)
crop_terrAlt <- crop(terrestrialAltitude, extent(south_america))

#Check that extents match
extent(south_america)
extent(crop_terrAlt)

#Extract points only from South America
Tmathaei_extract <- extract(crop_terrAlt, Tmathaei_spatial)

#Link complete dataset with extracted point yes/no
Tmathaei_extract<- cbind(Tmathaei_clean, Tmathaei_extract)
Tmathaei_extract

#Remove incomplete cases (those not in extracted dataset)
clean_Tmathaei <- Tmathaei_extract[complete.cases(Tmathaei_extract[,4]),]

#Get resolution of raster of environmental data
rasterResolution <- max(res(terrestrialAltitude))
rasterResolution

#Reduce resolution
while(min(nndist(clean_Tmathaei[,2:3])) < rasterResolution){
  nnD <- nndist(clean_Tmathaei[,2:3])
  clean_Tmathaei <- clean_Tmathaei[-(which(min(nnD) == nnD)[1]),]
}

clean_Tmathaei

#Set rownames of clean dataset
row.names(clean_Tmathaei) <- seq(nrow(clean_Tmathaei))


#Check for outliers by plotting
plot_Tmathaei <- clean_Tmathaei
coordinates(plot_Tmathaei) <- c("longitude", "latitude")
proj4string(plot_Tmathaei) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

plot(south_america)
plot(plot_Tmathaei, add=TRUE, col = "purple")

#Save the clean data
write.csv(clean_Tmathaei, "./Data/Cleaned_occurrences_by_species/Tmathaei_cleaned_occurrences.csv", row.names = F)
