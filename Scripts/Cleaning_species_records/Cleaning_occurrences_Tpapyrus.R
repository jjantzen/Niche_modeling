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

Tpapyrus <- read.csv("./Data/Occurrence_records_by_species/Tpapyrus_unique.csv", stringsAsFactors = FALSE)
Tpapyrus <- Tpapyrus[,-c(2:5)]

colnames(Tpapyrus)[1] <- "name"

head(Tpapyrus)

Tpapyrus_clean <- coord_impossible(Tpapyrus)
nrow(Tpapyrus_clean)
#Remove points with no environmental data

#Make data spatial
Tpapyrus_spatial <- Tpapyrus_clean
coordinates(Tpapyrus_spatial) <- c("longitude", "latitude")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
proj4string(Tpapyrus_spatial) <- crs.geo


#Import environmental data
terrestrialAltitude <- raster("./Layers/terrestrialAltitude.asc")
south_america <- readOGR("./Layers/joined.shp")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
crs(terrestrialAltitude) <- crs.geo

crs(south_america)
crs(terrestrialAltitude) 
crs(Tpapyrus_spatial)

#Crop terrestrialAltitude to geographic area (South America)
crop_terrAlt <- crop(terrestrialAltitude, extent(south_america))

#Check that extents match
extent(south_america)
extent(crop_terrAlt)

#Extract points only from South America
Tpapyrus_extract <- extract(crop_terrAlt, Tpapyrus_spatial)

#Link complete dataset with extracted point yes/no
Tpapyrus_extract<- cbind(Tpapyrus_clean, Tpapyrus_extract)
Tpapyrus_extract

#Remove incomplete cases (those not in extracted dataset)
clean_Tpapyrus <- Tpapyrus_extract[complete.cases(Tpapyrus_extract[,4]),]

#Get resolution of raster of environmental data
rasterResolution <- max(res(terrestrialAltitude))
rasterResolution

#Reduce resolution
while(min(nndist(clean_Tpapyrus[,2:3])) < rasterResolution){
  nnD <- nndist(clean_Tpapyrus[,2:3])
  clean_Tpapyrus <- clean_Tpapyrus[-(which(min(nnD) == nnD)[1]),]
}

clean_Tpapyrus

#Set rownames of clean dataset
row.names(clean_Tpapyrus) <- seq(nrow(clean_Tpapyrus))


#Check for outliers by plotting
plot_Tpapyrus <- clean_Tpapyrus
coordinates(plot_Tpapyrus) <- c("longitude", "latitude")
proj4string(plot_Tpapyrus) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

plot(south_america)
plot(plot_Tpapyrus, add=TRUE, col = "purple")


#Save the clean data
write.csv(clean_Tpapyrus, "./Data/Cleaned_occurrences_by_species/Tpapyrus_cleaned_occurrences.csv", row.names = F)
