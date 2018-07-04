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

Tmelastomoides <- read.csv("./Data/Occurrence_records_by_species/Tmelastomoides_unique.csv", stringsAsFactors = FALSE)
Tmelastomoides <- Tmelastomoides[,-c(2:5)]

colnames(Tmelastomoides)[1] <- "name"

head(Tmelastomoides)

Tmelastomoides_clean <- coord_impossible(Tmelastomoides)
nrow(Tmelastomoides_clean)
#Remove points with no environmental data

#Make data spatial
Tmelastomoides_spatial <- Tmelastomoides_clean
coordinates(Tmelastomoides_spatial) <- c("longitude", "latitude")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
proj4string(Tmelastomoides_spatial) <- crs.geo


#Import environmental data
terrestrialAltitude <- raster("./Layers/terrestrialAltitude.asc")
south_america <- readOGR("./Layers/joined.shp")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
crs(terrestrialAltitude) <- crs.geo

crs(south_america)
crs(terrestrialAltitude) 
crs(Tmelastomoides_spatial)

#Crop terrestrialAltitude to geographic area (South America)
crop_terrAlt <- crop(terrestrialAltitude, extent(south_america))

#Check that extents match
extent(south_america)
extent(crop_terrAlt)

#Extract points only from South America
Tmelastomoides_extract <- extract(crop_terrAlt, Tmelastomoides_spatial)

#Link complete dataset with extracted point yes/no
Tmelastomoides_extract<- cbind(Tmelastomoides_clean, Tmelastomoides_extract)
Tmelastomoides_extract

#Remove incomplete cases (those not in extracted dataset)
clean_Tmelastomoides <- Tmelastomoides_extract[complete.cases(Tmelastomoides_extract[,4]),]

#Get resolution of raster of environmental data
rasterResolution <- max(res(terrestrialAltitude))
rasterResolution

#Reduce resolution
while(min(nndist(clean_Tmelastomoides[,2:3])) < rasterResolution){
  nnD <- nndist(clean_Tmelastomoides[,2:3])
  clean_Tmelastomoides <- clean_Tmelastomoides[-(which(min(nnD) == nnD)[1]),]
}

clean_Tmelastomoides

#Set rownames of clean dataset
row.names(clean_Tmelastomoides) <- seq(nrow(clean_Tmelastomoides))


#Check for outliers by plotting
plot_Tmelastomoides <- clean_Tmelastomoides
coordinates(plot_Tmelastomoides) <- c("longitude", "latitude")
proj4string(plot_Tmelastomoides) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

plot(south_america)
plot(plot_Tmelastomoides, add=TRUE, col = "purple")

#Save the clean data
write.csv(clean_Tmelastomoides, "./Data/Cleaned_occurrences_by_species/Tmelastomoides_cleaned_occurrences.csv", row.names = F)





#### For after adding field collections in (and NYBG specimens)
mod_Tmel <- read.csv("./Data/Cleaned_occurrences_by_species/Tmelastomoides_cleaned_occurrences_MOD.csv", stringsAsFactors = FALSE)

mod_Tmel
mod_Tmel_clean <- coord_impossible(mod_Tmel)
mod_Tmel
mod_Tmel_clean

mod_Tmel_clean_spatial <- mod_Tmel_clean
coordinates(mod_Tmel_clean_spatial) <- c("longitude", "latitude")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
proj4string(mod_Tmel_clean_spatial) <- crs.geo

#Extract points only from South America
Tmel_mod_extract <- extract(crop_terrAlt, mod_Tmel_clean_spatial)

#Link complete dataset with extracted point yes/no
Tmel_mod_extract<- cbind(mod_Tmel_clean, Tmel_mod_extract)
Tmel_mod_extract

#Remove incomplete cases (those not in extracted dataset)
clean_Tmel_mod <- Tmel_mod_extract[complete.cases(Tmel_mod_extract[,4]),]
clean_Tmel_mod
#Get resolution of raster of environmental data
rasterResolution <- max(res(terrestrialAltitude))
rasterResolution

#Reduce resolution
while(min(nndist(clean_Tmel_mod[,2:3])) < rasterResolution){
  nnD <- nndist(clean_Tmel_mod[,2:3])
  clean_Tmel_mod <- clean_Tmel_mod[-(which(min(nnD) == nnD)[1]),]
}

clean_Tmel_mod
#####here 
#Set rownames of clean dataset
row.names(clean_Tmel_mod) <- seq(nrow(clean_Tmel_mod))


#Check for outliers by plotting
plot_Tmel_mod <- clean_Tmel_mod
coordinates(plot_Tmel_mod) <- c("longitude", "latitude")
proj4string(plot_Tmel_mod) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

plot(south_america)
plot(plot_Tmel_mod, add=TRUE, col = "purple")

write.csv(clean_Tmel_mod, "./Data/Cleaned_occurrences_by_species/Melastomoides_modified_postcleaning.csv", row.names = F)
