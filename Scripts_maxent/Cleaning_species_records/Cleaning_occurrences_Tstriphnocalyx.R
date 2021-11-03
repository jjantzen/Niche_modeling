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

Tstriphnocalyx <- read.csv("./Data/Occurrence_records_by_species/Tstriphnocalyx_unique.csv", stringsAsFactors = FALSE)
Tstriphnocalyx <- Tstriphnocalyx[,-c(2:5)]

colnames(Tstriphnocalyx)[1] <- "name"

head(Tstriphnocalyx)

Tstriphnocalyx_clean <- coord_impossible(Tstriphnocalyx)
nrow(Tstriphnocalyx_clean)
#Remove points with no environmental data

#Make data spatial
Tstriphnocalyx_spatial <- Tstriphnocalyx_clean
coordinates(Tstriphnocalyx_spatial) <- c("longitude", "latitude")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
proj4string(Tstriphnocalyx_spatial) <- crs.geo


#Import environmental data
terrestrialAltitude <- raster("./Layers/terrestrialAltitude.asc")
south_america <- readOGR("./Layers/joined.shp")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
crs(terrestrialAltitude) <- crs.geo

crs(south_america)
crs(terrestrialAltitude) 
crs(Tstriphnocalyx_spatial)

#Crop terrestrialAltitude to geographic area (South America)
crop_terrAlt <- crop(terrestrialAltitude, extent(south_america))

#Check that extents match
extent(south_america)
extent(crop_terrAlt)

#Extract points only from South America
Tstriphnocalyx_extract <- extract(crop_terrAlt, Tstriphnocalyx_spatial)

#Link complete dataset with extracted point yes/no
Tstriphnocalyx_extract<- cbind(Tstriphnocalyx_clean, Tstriphnocalyx_extract)
Tstriphnocalyx_extract

#Remove incomplete cases (those not in extracted dataset)
clean_Tstriphnocalyx <- Tstriphnocalyx_extract[complete.cases(Tstriphnocalyx_extract[,4]),]

#Get resolution of raster of environmental data
rasterResolution <- max(res(terrestrialAltitude))
rasterResolution

#Reduce resolution
while(min(nndist(clean_Tstriphnocalyx[,2:3])) < rasterResolution){
  nnD <- nndist(clean_Tstriphnocalyx[,2:3])
  clean_Tstriphnocalyx <- clean_Tstriphnocalyx[-(which(min(nnD) == nnD)[1]),]
}

clean_Tstriphnocalyx

#Set rownames of clean dataset
row.names(clean_Tstriphnocalyx) <- seq(nrow(clean_Tstriphnocalyx))


#Check for outliers by plotting
plot_Tstriphnocalyx <- clean_Tstriphnocalyx
coordinates(plot_Tstriphnocalyx) <- c("longitude", "latitude")
proj4string(plot_Tstriphnocalyx) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

plot(south_america)
plot(plot_Tstriphnocalyx, add=TRUE, col = "purple")

#Save the clean data
write.csv(clean_Tstriphnocalyx, "./Data/Cleaned_occurrences_by_species/Tstriphnocalyx_cleaned_occurrences.csv", row.names = F)
