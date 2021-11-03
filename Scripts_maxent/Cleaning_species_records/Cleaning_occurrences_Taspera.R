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

Taspera <- read.csv("./Data/Occurrence_records_by_species/Taspera_unique.csv", stringsAsFactors = FALSE)
Taspera <- Taspera[,-c(2:5)]
#Remove duplicates
#Taegopogon <- unique(Taegopogon[,-2])
Taspera

#Set species epithet to name
colnames(Taspera)[1] <- "name"

head(Taspera)

Taspera_clean <- coord_impossible(Taspera)
nrow(Taspera_clean)
#Remove points with no environmental data

#Make data spatial
Taspera_spatial <- Taspera_clean
coordinates(Taspera_spatial) <- c("longitude", "latitude")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
proj4string(Taspera_spatial) <- crs.geo


#Import environmental data
terrestrialAltitude <- raster("./Layers/terrestrialAltitude.asc")
south_america <- readOGR("./Layers/joined.shp")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
crs(terrestrialAltitude) <- crs.geo

crs(south_america)
crs(terrestrialAltitude) 
crs(Taspera_spatial)

#Crop terrestrialAltitude to geographic area (South America)
crop_terrAlt <- crop(terrestrialAltitude, extent(south_america))

#Check that extents match
extent(south_america)
extent(crop_terrAlt)

#Extract points only from South America
Taspera_extract <- extract(crop_terrAlt, Taspera_spatial)

#Link complete dataset with extracted point yes/no
Taspera_extract<- cbind(Taspera_clean, Taspera_extract)
Taspera_extract

#Remove incomplete cases (those not in extracted dataset)
clean_Taspera <- Taspera_extract[complete.cases(Taspera_extract[,4]),]

#Get resolution of raster of environmental data
rasterResolution <- max(res(terrestrialAltitude))
rasterResolution

#Reduce resolution
while(min(nndist(clean_Taspera[,2:3])) < rasterResolution){
  nnD <- nndist(clean_Taspera[,2:3])
  clean_Taspera <- clean_Taspera[-(which(min(nnD) == nnD)[1]),]
}


#Set rownames of clean dataset
row.names(clean_Taspera) <- seq(nrow(clean_Taspera))

clean_Taspera
#Check for outliers by plotting
plot_Taspera <- clean_Taspera
coordinates(plot_Taspera) <- c("longitude", "latitude")
proj4string(plot_Taspera) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

plot(south_america)
plot(plot_Taspera, add=TRUE, col = "purple")


#Save the clean data
write.csv(clean_Taspera[,1:3], "./Data/Cleaned_occurrences_by_species/Taspera_cleaned_occurrences.csv", row.names = F)



#### For after adding field collections in (and NYBG specimens)
mod_Tasp <- read.csv("./Data/Cleaned_occurrences_by_species/Taspera_cleaned_occurrences_MOD.csv", stringsAsFactors = FALSE)

mod_Tasp
mod_Tasp_clean <- coord_impossible(mod_Tasp)
mod_Tasp
mod_Tasp_clean

mod_Tasp_clean_spatial <- mod_Tasp_clean
coordinates(mod_Tasp_clean_spatial) <- c("longitude", "latitude")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
proj4string(mod_Tasp_clean_spatial) <- crs.geo

#Extract points only from South America
Tasp_mod_extract <- extract(crop_terrAlt, mod_Tasp_clean_spatial)

#Link complete dataset with extracted point yes/no
Tasp_mod_extract<- cbind(mod_Tasp_clean, Tasp_mod_extract)
Tasp_mod_extract

#Remove incomplete cases (those not in extracted dataset)
clean_Tasp_mod <- Tasp_mod_extract[complete.cases(Tasp_mod_extract[,4]),]
clean_Tasp_mod
#Get resolution of raster of environmental data
rasterResolution <- max(res(terrestrialAltitude))
rasterResolution

#Reduce resolution
while(min(nndist(clean_Tasp_mod[,2:3])) < rasterResolution){
  nnD <- nndist(clean_Tasp_mod[,2:3])
  clean_Tasp_mod <- clean_Tasp_mod[-(which(min(nnD) == nnD)[1]),]
}


#####here 
#Set rownames of clean dataset
row.names(clean_Tasp_mod) <- seq(nrow(clean_Tasp_mod))
clean_Tasp_mod

#Check for outliers by plotting
plot_Tasp_mod <- clean_Tasp_mod
coordinates(plot_Tasp_mod) <- c("longitude", "latitude")
proj4string(plot_Tasp_mod) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

plot(south_america)
plot(plot_Tasp_mod, add=TRUE, col = "purple")




write.csv(clean_Tasp_mod, "./Data/Cleaned_occurrences_by_species/Aspera_modified_postcleaning.csv", row.names = F)
