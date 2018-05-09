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

Tbarbigera <- read.csv("./Data/Occurrence_records_by_species/Tbarbigera_unique.csv", stringsAsFactors = FALSE)
Tbarbigera <- Tbarbigera[,-c(2:5)]

colnames(Tbarbigera)[1] <- "name"

head(Tbarbigera)

Tbarbigera_clean <- coord_impossible(Tbarbigera)
nrow(Tbarbigera_clean)
#Remove points with no environmental data

#Make data spatial
Tbarbigera_spatial <- Tbarbigera_clean
coordinates(Tbarbigera_spatial) <- c("longitude", "latitude")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
proj4string(Tbarbigera_spatial) <- crs.geo


#Import environmental data
terrestrialAltitude <- raster("./Layers/terrestrialAltitude.asc")
south_america <- readOGR("./Layers/joined.shp")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
crs(terrestrialAltitude) <- crs.geo

crs(south_america)
crs(terrestrialAltitude) 
crs(Tbarbigera_spatial)

#Crop terrestrialAltitude to geographic area (South America)
crop_terrAlt <- crop(terrestrialAltitude, extent(south_america))

#Check that extents match
extent(south_america)
extent(crop_terrAlt)

#Extract points only from South America
Tbarbigera_extract <- extract(crop_terrAlt, Tbarbigera_spatial)

#Link complete dataset with extracted point yes/no
Tbarbigera_extract<- cbind(Tbarbigera_clean, Tbarbigera_extract)
Tbarbigera_extract

#Remove incomplete cases (those not in extracted dataset)
clean_Tbarbigera <- Tbarbigera_extract[complete.cases(Tbarbigera_extract[,4]),]

#Get resolution of raster of environmental data
rasterResolution <- max(res(terrestrialAltitude))
rasterResolution

#Reduce resolution
while(min(nndist(clean_Tbarbigera[,2:3])) < rasterResolution){
  nnD <- nndist(clean_Tbarbigera[,2:3])
  clean_Tbarbigera <- clean_Tbarbigera[-(which(min(nnD) == nnD)[1]),]
}

clean_Tbarbigera

#Set rownames of clean dataset
row.names(clean_Tbarbigera) <- seq(nrow(clean_Tbarbigera))


#Check for outliers by plotting
plot_Tbarbigera <- clean_Tbarbigera
coordinates(plot_Tbarbigera) <- c("longitude", "latitude")
proj4string(plot_Tbarbigera) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

plot(south_america)
plot(plot_Tbarbigera, add=TRUE, col = "purple")

#Save the clean data
write.csv(clean_Tbarbigera, "./Data/Cleaned_occurrences_by_species/Tbarbigera_cleaned_occurrences.csv", row.names = F)
