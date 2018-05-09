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

Tllanorum <- read.csv("./Data/Occurrence_records_by_species/Tllanorum_unique.csv", stringsAsFactors = FALSE)
Tllanorum <- Tllanorum[,-c(2:5)]

colnames(Tllanorum)[1] <- "name"

head(Tllanorum)

Tllanorum_clean <- coord_impossible(Tllanorum)
nrow(Tllanorum_clean)
#Remove points with no environmental data

#Make data spatial
Tllanorum_spatial <- Tllanorum_clean
coordinates(Tllanorum_spatial) <- c("longitude", "latitude")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
proj4string(Tllanorum_spatial) <- crs.geo


#Import environmental data
terrestrialAltitude <- raster("./Layers/terrestrialAltitude.asc")
south_america <- readOGR("./Layers/joined.shp")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
crs(terrestrialAltitude) <- crs.geo

crs(south_america)
crs(terrestrialAltitude) 
crs(Tllanorum_spatial)

#Crop terrestrialAltitude to geographic area (South America)
crop_terrAlt <- crop(terrestrialAltitude, extent(south_america))

#Check that extents match
extent(south_america)
extent(crop_terrAlt)

#Extract points only from South America
Tllanorum_extract <- extract(crop_terrAlt, Tllanorum_spatial)

#Link complete dataset with extracted point yes/no
Tllanorum_extract<- cbind(Tllanorum_clean, Tllanorum_extract)
Tllanorum_extract

#Remove incomplete cases (those not in extracted dataset)
clean_Tllanorum <- Tllanorum_extract[complete.cases(Tllanorum_extract[,4]),]

#Get resolution of raster of environmental data
rasterResolution <- max(res(terrestrialAltitude))
rasterResolution

#Reduce resolution
while(min(nndist(clean_Tllanorum[,2:3])) < rasterResolution){
  nnD <- nndist(clean_Tllanorum[,2:3])
  clean_Tllanorum <- clean_Tllanorum[-(which(min(nnD) == nnD)[1]),]
}

clean_Tllanorum

#Set rownames of clean dataset
row.names(clean_Tllanorum) <- seq(nrow(clean_Tllanorum))


#Check for outliers by plotting
plot_Tllanorum <- clean_Tllanorum
coordinates(plot_Tllanorum) <- c("longitude", "latitude")
proj4string(plot_Tllanorum) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

plot(south_america)
plot(plot_Tllanorum, add=TRUE, col = "purple")


ordered_points <- clean_Tllanorum %>% 
  arrange(latitude)

ordered_points

#Save the clean data
write.csv(clean_Tllanorum, "./Data/Cleaned_occurrences_by_species/Tllanorum_cleaned_occurrences.csv", row.names = F)
