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

Tedmundoi <- read.csv("./Data/Occurrence_records_by_species/Tedmundoi_unique.csv", stringsAsFactors = FALSE)
Tedmundoi <- Tedmundoi[,-c(2:5)]

colnames(Tedmundoi)[1] <- "name"

head(Tedmundoi)

Tedmundoi_clean <- coord_impossible(Tedmundoi)
nrow(Tedmundoi_clean)
#Remove points with no environmental data

#Make data spatial
Tedmundoi_spatial <- Tedmundoi_clean
coordinates(Tedmundoi_spatial) <- c("longitude", "latitude")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
proj4string(Tedmundoi_spatial) <- crs.geo


#Import environmental data
terrestrialAltitude <- raster("./Layers/terrestrialAltitude.asc")
south_america <- readOGR("./Layers/joined.shp")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
crs(terrestrialAltitude) <- crs.geo

crs(south_america)
crs(terrestrialAltitude) 
crs(Tedmundoi_spatial)

#Crop terrestrialAltitude to geographic area (South America)
crop_terrAlt <- crop(terrestrialAltitude, extent(south_america))

#Check that extents match
extent(south_america)
extent(crop_terrAlt)

#Extract points only from South America
Tedmundoi_extract <- extract(crop_terrAlt, Tedmundoi_spatial)

#Link complete dataset with extracted point yes/no
Tedmundoi_extract<- cbind(Tedmundoi_clean, Tedmundoi_extract)
Tedmundoi_extract

#Remove incomplete cases (those not in extracted dataset)
clean_Tedmundoi <- Tedmundoi_extract[complete.cases(Tedmundoi_extract[,4]),]

#Get resolution of raster of environmental data
rasterResolution <- max(res(terrestrialAltitude))
rasterResolution

#Reduce resolution
while(min(nndist(clean_Tedmundoi[,2:3])) < rasterResolution){
  nnD <- nndist(clean_Tedmundoi[,2:3])
  clean_Tedmundoi <- clean_Tedmundoi[-(which(min(nnD) == nnD)[1]),]
}

clean_Tedmundoi

#Set rownames of clean dataset
row.names(clean_Tedmundoi) <- seq(nrow(clean_Tedmundoi))


#Check for outliers by plotting
plot_Tedmundoi <- clean_Tedmundoi
coordinates(plot_Tedmundoi) <- c("longitude", "latitude")
proj4string(plot_Tedmundoi) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

plot(south_america)
plot(plot_Tedmundoi, add=TRUE, col = "purple")

#Save the clean data
write.csv(clean_Tedmundoi, "./Data/Cleaned_occurrences_by_species/Tedmundoi_cleaned_occurrences.csv", row.names = F)
