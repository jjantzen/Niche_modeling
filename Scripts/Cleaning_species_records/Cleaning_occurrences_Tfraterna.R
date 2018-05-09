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

Tfraterna <- read.csv("./Data/Occurrence_records_by_species/Tfraterna_unique.csv", stringsAsFactors = FALSE)
Tfraterna <- Tfraterna[,-c(2:5)]

colnames(Tfraterna)[1] <- "name"

head(Tfraterna)

Tfraterna_clean <- coord_impossible(Tfraterna)
nrow(Tfraterna_clean)
#Remove points with no environmental data

#Make data spatial
Tfraterna_spatial <- Tfraterna_clean
coordinates(Tfraterna_spatial) <- c("longitude", "latitude")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
proj4string(Tfraterna_spatial) <- crs.geo


#Import environmental data
terrestrialAltitude <- raster("./Layers/terrestrialAltitude.asc")
south_america <- readOGR("./Layers/joined.shp")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
crs(terrestrialAltitude) <- crs.geo

crs(south_america)
crs(terrestrialAltitude) 
crs(Tfraterna_spatial)

#Crop terrestrialAltitude to geographic area (South America)
crop_terrAlt <- crop(terrestrialAltitude, extent(south_america))

#Check that extents match
extent(south_america)
extent(crop_terrAlt)

#Extract points only from South America
Tfraterna_extract <- extract(crop_terrAlt, Tfraterna_spatial)

#Link complete dataset with extracted point yes/no
Tfraterna_extract<- cbind(Tfraterna_clean, Tfraterna_extract)
Tfraterna_extract

#Remove incomplete cases (those not in extracted dataset)
clean_Tfraterna <- Tfraterna_extract[complete.cases(Tfraterna_extract[,4]),]

#Get resolution of raster of environmental data
rasterResolution <- max(res(terrestrialAltitude))
rasterResolution

#Reduce resolution
while(min(nndist(clean_Tfraterna[,2:3])) < rasterResolution){
  nnD <- nndist(clean_Tfraterna[,2:3])
  clean_Tfraterna <- clean_Tfraterna[-(which(min(nnD) == nnD)[1]),]
}

clean_Tfraterna

#Set rownames of clean dataset
row.names(clean_Tfraterna) <- seq(nrow(clean_Tfraterna))


#Check for outliers by plotting
plot_Tfraterna <- clean_Tfraterna
coordinates(plot_Tfraterna) <- c("longitude", "latitude")
proj4string(plot_Tfraterna) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

plot(south_america)
plot(plot_Tfraterna, add=TRUE, col = "purple")

#Save the clean data
write.csv(clean_Tfraterna, "./Data/Cleaned_occurrences_by_species/Tfraterna_cleaned_occurrences.csv", row.names = F)
