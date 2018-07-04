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

Tduidae <- read.csv("./Data/Occurrence_records_by_species/Tduidae_unique.csv", stringsAsFactors = FALSE)
Tduidae <- Tduidae[,-c(2:5)]

colnames(Tduidae)[1] <- "name"

head(Tduidae)

Tduidae_clean <- coord_impossible(Tduidae)
nrow(Tduidae_clean)
#Remove points with no environmental data

#Make data spatial
Tduidae_spatial <- Tduidae_clean
coordinates(Tduidae_spatial) <- c("longitude", "latitude")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
proj4string(Tduidae_spatial) <- crs.geo


#Import environmental data
terrestrialAltitude <- raster("./Layers/terrestrialAltitude.asc")
south_america <- readOGR("./Layers/joined.shp")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
crs(terrestrialAltitude) <- crs.geo

crs(south_america)
crs(terrestrialAltitude) 
crs(Tduidae_spatial)

#Crop terrestrialAltitude to geographic area (South America)
crop_terrAlt <- crop(terrestrialAltitude, extent(south_america))

#Check that extents match
extent(south_america)
extent(crop_terrAlt)

#Extract points only from South America
Tduidae_extract <- extract(crop_terrAlt, Tduidae_spatial)

#Link complete dataset with extracted point yes/no
Tduidae_extract<- cbind(Tduidae_clean, Tduidae_extract)
Tduidae_extract

#Remove incomplete cases (those not in extracted dataset)
clean_Tduidae <- Tduidae_extract[complete.cases(Tduidae_extract[,4]),]

#Get resolution of raster of environmental data
rasterResolution <- max(res(terrestrialAltitude))
rasterResolution

#Reduce resolution
while(min(nndist(clean_Tduidae[,2:3])) < rasterResolution){
  nnD <- nndist(clean_Tduidae[,2:3])
  clean_Tduidae <- clean_Tduidae[-(which(min(nnD) == nnD)[1]),]
}

clean_Tduidae

#Set rownames of clean dataset
row.names(clean_Tduidae) <- seq(nrow(clean_Tduidae))


#Check for outliers by plotting
plot_Tduidae <- clean_Tduidae
coordinates(plot_Tduidae) <- c("longitude", "latitude")
proj4string(plot_Tduidae) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

plot(south_america)
plot(plot_Tduidae, add=TRUE, col = "purple")

#Save the clean data
write.csv(clean_Tduidae, "./Data/Cleaned_occurrences_by_species/Tduidae_cleaned_occurrences.csv", row.names = F)





#### For after adding field collections in (and NYBG specimens)
mod_Tduid <- read.csv("./Data/Cleaned_occurrences_by_species/Tduidae_cleaned_occurrences_MOD.csv", stringsAsFactors = FALSE)

mod_Tduid
mod_Tduid_clean <- coord_impossible(mod_Tduid)
mod_Tduid
mod_Tduid_clean

mod_Tduid_clean_spatial <- mod_Tduid_clean
coordinates(mod_Tduid_clean_spatial) <- c("longitude", "latitude")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
proj4string(mod_Tduid_clean_spatial) <- crs.geo

#Extract points only from South America
Tduid_mod_extract <- extract(crop_terrAlt, mod_Tduid_clean_spatial)

#Link complete dataset with extracted point yes/no
Tduid_mod_extract<- cbind(mod_Tduid_clean, Tduid_mod_extract)
Tduid_mod_extract

#Remove incomplete cases (those not in extracted dataset)
clean_Tduid_mod <- Tduid_mod_extract[complete.cases(Tduid_mod_extract[,4]),]
clean_Tduid_mod
#Get resolution of raster of environmental data
rasterResolution <- max(res(terrestrialAltitude))
rasterResolution

#Reduce resolution
while(min(nndist(clean_Tduid_mod[,2:3])) < rasterResolution){
  nnD <- nndist(clean_Tduid_mod[,2:3])
  clean_Tduid_mod <- clean_Tduid_mod[-(which(min(nnD) == nnD)[1]),]
}

clean_Tduid_mod
#####here 
#Set rownames of clean dataset
row.names(clean_Tduid_mod) <- seq(nrow(clean_Tduid_mod))


#Check for outliers by plotting
plot_Tduid_mod <- clean_Tduid_mod
coordinates(plot_Tduid_mod) <- c("longitude", "latitude")
proj4string(plot_Tduid_mod) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

plot(south_america)
plot(plot_Tduid_mod, add=TRUE, col = "purple")

write.csv(clean_Tduid_mod, "./Data/Cleaned_occurrences_by_species/Duidae_modified_postcleaning.csv", row.names = F)
