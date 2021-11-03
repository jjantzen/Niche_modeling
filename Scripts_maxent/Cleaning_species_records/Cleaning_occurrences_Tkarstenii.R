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

Tkarstenii <- read.csv("./Data/Occurrence_records_by_species/Tkarstenii_unique.csv", stringsAsFactors = FALSE)
Tkarstenii <- Tkarstenii[,-c(2:5)]

colnames(Tkarstenii)[1] <- "name"

head(Tkarstenii)

Tkarstenii_clean <- coord_impossible(Tkarstenii)
nrow(Tkarstenii_clean)
#Remove points with no environmental data

#Make data spatial
Tkarstenii_spatial <- Tkarstenii_clean
coordinates(Tkarstenii_spatial) <- c("longitude", "latitude")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
proj4string(Tkarstenii_spatial) <- crs.geo


#Import environmental data
terrestrialAltitude <- raster("./Layers/terrestrialAltitude.asc")
south_america <- readOGR("./Layers/joined.shp")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
crs(terrestrialAltitude) <- crs.geo

crs(south_america)
crs(terrestrialAltitude) 
crs(Tkarstenii_spatial)

#Crop terrestrialAltitude to geographic area (South America)
crop_terrAlt <- crop(terrestrialAltitude, extent(south_america))

#Check that extents match
extent(south_america)
extent(crop_terrAlt)

#Extract points only from South America
Tkarstenii_extract <- extract(crop_terrAlt, Tkarstenii_spatial)

#Link complete dataset with extracted point yes/no
Tkarstenii_extract<- cbind(Tkarstenii_clean, Tkarstenii_extract)
Tkarstenii_extract

#Remove incomplete cases (those not in extracted dataset)
clean_Tkarstenii <- Tkarstenii_extract[complete.cases(Tkarstenii_extract[,4]),]

#Get resolution of raster of environmental data
rasterResolution <- max(res(terrestrialAltitude))
rasterResolution

#Reduce resolution
while(min(nndist(clean_Tkarstenii[,2:3])) < rasterResolution){
  nnD <- nndist(clean_Tkarstenii[,2:3])
  clean_Tkarstenii <- clean_Tkarstenii[-(which(min(nnD) == nnD)[1]),]
}

clean_Tkarstenii

#Set rownames of clean dataset
row.names(clean_Tkarstenii) <- seq(nrow(clean_Tkarstenii))


#Check for outliers by plotting
plot_Tkarstenii <- clean_Tkarstenii
coordinates(plot_Tkarstenii) <- c("longitude", "latitude")
proj4string(plot_Tkarstenii) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

plot(south_america)
plot(plot_Tkarstenii, add=TRUE, col = "purple")

#Save the clean data
write.csv(clean_Tkarstenii, "./Data/Cleaned_occurrences_by_species/Tkarstenii_cleaned_occurrences.csv", row.names = F)
