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

Tcath <- read.csv("./Data/Occurrence_records_by_species/Tcatharinae_unique.csv", stringsAsFactors = FALSE)
Tcath <- Tcath[-c(7:9),-c(2:5)]

Tcath

#Set species epithet to name
colnames(Tcath)[1] <- "name"

head(Tcath)

Tcath_clean <- coord_impossible(Tcath)
nrow(Tcath_clean)
#Remove points with no environmental data

#Make data spatial
Tcath_spatial <- Tcath_clean
coordinates(Tcath_spatial) <- c("longitude", "latitude")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
proj4string(Tcath_spatial) <- crs.geo


#Import environmental data
terrestrialAltitude <- raster("./Layers/terrestrialAltitude.asc")
south_america <- readOGR("./Layers/joined.shp")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
crs(terrestrialAltitude) <- crs.geo

crs(south_america)
crs(terrestrialAltitude) 
crs(Tcath_spatial)

#Crop terrestrialAltitude to geographic area (South America)
crop_terrAlt <- crop(terrestrialAltitude, extent(south_america))

#Check that extents match
extent(south_america)
extent(crop_terrAlt)

#Extract points only from South America
Tcath_extract <- extract(crop_terrAlt, Tcath_spatial)

#Link complete dataset with extracted point yes/no
Tcath_extract<- cbind(Tcath_clean, Tcath_extract)
Tcath_extract

#Remove incomplete cases (those not in extracted dataset)
clean_Tcath <- Tcath_extract[complete.cases(Tcath_extract[,4]),]

#Get resolution of raster of environmental data
rasterResolution <- max(res(terrestrialAltitude))
rasterResolution

#Reduce resolution
while(min(nndist(clean_Tcath[,2:3])) < rasterResolution){
  nnD <- nndist(clean_Tcath[,2:3])
  clean_Tcath <- clean_Tcath[-(which(min(nnD) == nnD)[1]),]
}

clean_Tcath

#Set rownames of clean dataset
row.names(clean_Tcath) <- seq(nrow(clean_Tcath))


#Check for outliers by plotting
plot_Tcath <- clean_Tcath
coordinates(plot_Tcath) <- c("longitude", "latitude")
proj4string(plot_Tcath) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

plot(south_america)
plot(plot_Tcath, add=TRUE, col = "purple")


#Save the clean data
write.csv(clean_Tcath, "./Data/Cleaned_occurrences_by_species/Tcatharinae_cleaned_occurrences.csv", row.names = F)
