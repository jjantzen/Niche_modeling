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

Tjohn <- read.csv("./Data/Occurrence_records_by_species/Tjohnwurdackiana_unique.csv", stringsAsFactors = FALSE)
Tjohn <- Tjohn[,-c(2:5)]

colnames(Tjohn)[1] <- "name"

head(Tjohn)

Tjohn_clean <- coord_impossible(Tjohn)
nrow(Tjohn_clean)
#Remove points with no environmental data

#Make data spatial
Tjohn_spatial <- Tjohn_clean
coordinates(Tjohn_spatial) <- c("longitude", "latitude")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
proj4string(Tjohn_spatial) <- crs.geo


#Import environmental data
terrestrialAltitude <- raster("./Layers/terrestrialAltitude.asc")
south_america <- readOGR("./Layers/joined.shp")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
crs(terrestrialAltitude) <- crs.geo

crs(south_america)
crs(terrestrialAltitude) 
crs(Tjohn_spatial)

#Crop terrestrialAltitude to geographic area (South America)
crop_terrAlt <- crop(terrestrialAltitude, extent(south_america))

#Check that extents match
extent(south_america)
extent(crop_terrAlt)

#Extract points only from South America
Tjohn_extract <- extract(crop_terrAlt, Tjohn_spatial)

#Link complete dataset with extracted point yes/no
Tjohn_extract<- cbind(Tjohn_clean, Tjohn_extract)
Tjohn_extract

#Remove incomplete cases (those not in extracted dataset)
clean_Tjohn <- Tjohn_extract[complete.cases(Tjohn_extract[,4]),]

#Get resolution of raster of environmental data
rasterResolution <- max(res(terrestrialAltitude))
rasterResolution

#Reduce resolution
while(min(nndist(clean_Tjohn[,2:3])) < rasterResolution){
  nnD <- nndist(clean_Tjohn[,2:3])
  clean_Tjohn <- clean_Tjohn[-(which(min(nnD) == nnD)[1]),]
}

clean_Tjohn

#Set rownames of clean dataset
row.names(clean_Tjohn) <- seq(nrow(clean_Tjohn))


#Check for outliers by plotting
plot_Tjohn <- clean_Tjohn
coordinates(plot_Tjohn) <- c("longitude", "latitude")
proj4string(plot_Tjohn) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

plot(south_america)
plot(plot_Tjohn, add=TRUE, col = "purple")

#Save the clean data
write.csv(clean_Tjohn, "./Data/Cleaned_occurrences_by_species/Tjohn_cleaned_occurrences.csv", row.names = F)
