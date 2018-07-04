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

Tdissitiflora <- read.csv("./Data/Occurrence_records_by_species/Tdissitiflora_unique.csv", stringsAsFactors = FALSE)
Tdissitiflora <- Tdissitiflora[,-c(2:5)]

colnames(Tdissitiflora)[1] <- "name"

head(Tdissitiflora)

Tdissitiflora_clean <- coord_impossible(Tdissitiflora)
nrow(Tdissitiflora_clean)
#Remove points with no environmental data

#Make data spatial
Tdissitiflora_spatial <- Tdissitiflora_clean
coordinates(Tdissitiflora_spatial) <- c("longitude", "latitude")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
proj4string(Tdissitiflora_spatial) <- crs.geo


#Import environmental data
terrestrialAltitude <- raster("./Layers/terrestrialAltitude.asc")
south_america <- readOGR("./Layers/joined.shp")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
crs(terrestrialAltitude) <- crs.geo

crs(south_america)
crs(terrestrialAltitude) 
crs(Tdissitiflora_spatial)

#Crop terrestrialAltitude to geographic area (South America)
crop_terrAlt <- crop(terrestrialAltitude, extent(south_america))

#Check that extents match
extent(south_america)
extent(crop_terrAlt)

#Extract points only from South America
Tdissitiflora_extract <- extract(crop_terrAlt, Tdissitiflora_spatial)

#Link complete dataset with extracted point yes/no
Tdissitiflora_extract<- cbind(Tdissitiflora_clean, Tdissitiflora_extract)
Tdissitiflora_extract

#Remove incomplete cases (those not in extracted dataset)
clean_Tdissitiflora <- Tdissitiflora_extract[complete.cases(Tdissitiflora_extract[,4]),]

#Get resolution of raster of environmental data
rasterResolution <- max(res(terrestrialAltitude))
rasterResolution

#Reduce resolution
while(min(nndist(clean_Tdissitiflora[,2:3])) < rasterResolution){
  nnD <- nndist(clean_Tdissitiflora[,2:3])
  clean_Tdissitiflora <- clean_Tdissitiflora[-(which(min(nnD) == nnD)[1]),]
}

clean_Tdissitiflora

#Set rownames of clean dataset
row.names(clean_Tdissitiflora) <- seq(nrow(clean_Tdissitiflora))


#Check for outliers by plotting
plot_Tdissitiflora <- clean_Tdissitiflora
coordinates(plot_Tdissitiflora) <- c("longitude", "latitude")
proj4string(plot_Tdissitiflora) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

plot(south_america)
plot(plot_Tdissitiflora, add=TRUE, col = "purple")

#Save the clean data
write.csv(clean_Tdissitiflora, "./Data/Cleaned_occurrences_by_species/Tdissitiflora_cleaned_occurrences.csv", row.names = F)




#### For after adding field collections in (and NYBG specimens)
mod_Tdiss <- read.csv("./Data/Cleaned_occurrences_by_species/Tdissitiflora_cleaned_occurrences_MOD.csv", stringsAsFactors = FALSE)

mod_Tdiss
mod_Tdiss_clean <- coord_impossible(mod_Tdiss)
mod_Tdiss
mod_Tdiss_clean

mod_Tdiss_clean_spatial <- mod_Tdiss_clean
coordinates(mod_Tdiss_clean_spatial) <- c("longitude", "latitude")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
proj4string(mod_Tdiss_clean_spatial) <- crs.geo

#Extract points only from South America
Tdiss_mod_extract <- extract(crop_terrAlt, mod_Tdiss_clean_spatial)

#Link complete dataset with extracted point yes/no
Tdiss_mod_extract<- cbind(mod_Tdiss_clean, Tdiss_mod_extract)
Tdiss_mod_extract

#Remove incomplete cases (those not in extracted dataset)
clean_Tdiss_mod <- Tdiss_mod_extract[complete.cases(Tdiss_mod_extract[,4]),]
clean_Tdiss_mod
#Get resolution of raster of environmental data
rasterResolution <- max(res(terrestrialAltitude))
rasterResolution

#Reduce resolution
while(min(nndist(clean_Tdiss_mod[,2:3])) < rasterResolution){
  nnD <- nndist(clean_Tdiss_mod[,2:3])
  clean_Tdiss_mod <- clean_Tdiss_mod[-(which(min(nnD) == nnD)[1]),]
}

clean_Tdiss_mod
#####here 
#Set rownames of clean dataset
row.names(clean_Tdiss_mod) <- seq(nrow(clean_Tdiss_mod))


#Check for outliers by plotting
plot_Tdiss_mod <- clean_Tdiss_mod
coordinates(plot_Tdiss_mod) <- c("longitude", "latitude")
proj4string(plot_Tdiss_mod) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

plot(south_america)
plot(plot_Tdiss_mod, add=TRUE, col = "purple")

write.csv(clean_Tdiss_mod, "./Data/Cleaned_occurrences_by_species/Dissitiflora_modified_postcleaning.csv", row.names = F)
