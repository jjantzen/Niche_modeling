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

Talbescens <- read.csv("./Data/Occurrence_records_by_species/Talbescens_unique.csv", stringsAsFactors = FALSE)
Talbescens <- Talbescens[,-c(2:5)]
#Remove duplicates
#Taegopogon <- unique(Taegopogon[,-2])
Talbescens

#Set species epithet to name
colnames(Talbescens)[1] <- "name"

head(Talbescens)

Talbescens_clean <- coord_impossible(Talbescens)
nrow(Talbescens_clean)
#Remove points with no environmental data

#Make data spatial
Talbescens_spatial <- Talbescens_clean
coordinates(Talbescens_spatial) <- c("longitude", "latitude")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
proj4string(Talbescens_spatial) <- crs.geo


#Import environmental data
terrestrialAltitude <- raster("./Layers/terrestrialAltitude.asc")
south_america <- readOGR("./Layers/joined.shp")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
crs(terrestrialAltitude) <- crs.geo

crs(south_america)
crs(terrestrialAltitude) 
crs(Talbescens_spatial)

#Crop terrestrialAltitude to geographic area (South America)
crop_terrAlt <- crop(terrestrialAltitude, extent(south_america))

#Check that extents match
extent(south_america)
extent(crop_terrAlt)

#Extract points only from South America
Talbescens_extract <- extract(crop_terrAlt, Talbescens_spatial)

#Link complete dataset with extracted point yes/no
Talbescens_extract<- cbind(Talbescens_clean, Talbescens_extract)
Talbescens_extract

#Remove incomplete cases (those not in extracted dataset)
clean_Talbescens <- Talbescens_extract[complete.cases(Talbescens_extract[,4]),]

#Get resolution of raster of environmental data
rasterResolution <- max(res(terrestrialAltitude))
rasterResolution

#Reduce resolution
while(min(nndist(clean_Talbescens[,2:3])) < rasterResolution){
  nnD <- nndist(clean_Talbescens[,2:3])
  clean_Talbescens <- clean_Talbescens[-(which(min(nnD) == nnD)[1]),]
}


#Set rownames of clean dataset
row.names(clean_Talbescens) <- seq(nrow(clean_Talbescens))


#Check for outliers by plotting
plot_Talbescens <- clean_Talbescens
coordinates(plot_Talbescens) <- c("longitude", "latitude")
proj4string(plot_Talbescens) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

plot(south_america)
plot(plot_Talbescens, add=TRUE, col = "purple")

#To order points to remove specific outliers
ordered_points <- clean_Talbescens %>% 
  arrange(longitude)
head(ordered_points)
tail(ordered_points)

ordered_points_removed <- ordered_points[-c(1:3),]

coordinates(ordered_points_removed) <- c("longitude", "latitude")
proj4string(ordered_points_removed) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

plot(south_america)
plot(ordered_points_removed, add=TRUE, col = "purple")


#Save the clean data
write.csv(ordered_points_removed[,1:3], "./Data/Cleaned_occurrences_by_species/Talbescens_cleaned_occurrences.csv", row.names = F)


#### For after adding field collections in (and NYBG specimens)
mod_Talb <- read.csv("./Data/Cleaned_occurrences_by_species/Talbescens_cleaned_occurrences_MOD.csv", stringsAsFactors = FALSE)

mod_Talb
mod_Talb_clean <- coord_impossible(mod_Talb)
mod_Talb
mod_Talb_clean

mod_Talb_clean_spatial <- mod_Talb_clean
coordinates(mod_Talb_clean_spatial) <- c("longitude", "latitude")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
proj4string(mod_Talb_clean_spatial) <- crs.geo

#Extract points only from South America
Talb_mod_extract <- extract(crop_terrAlt, mod_Talb_clean_spatial)

#Link complete dataset with extracted point yes/no
Talb_mod_extract<- cbind(mod_Talb_clean, Talb_mod_extract)
Talb_mod_extract

#Remove incomplete cases (those not in extracted dataset)
clean_Talb_mod <- Talb_mod_extract[complete.cases(Talb_mod_extract[,4]),]
clean_Talb_mod
#Get resolution of raster of environmental data
rasterResolution <- max(res(terrestrialAltitude))
rasterResolution

#Reduce resolution
while(min(nndist(clean_Talb_mod[,2:3])) < rasterResolution){
  nnD <- nndist(clean_Talb_mod[,2:3])
  clean_Talb_mod <- clean_Talb_mod[-(which(min(nnD) == nnD)[1]),]
}

clean_Talb_mod
#####here 
#Set rownames of clean dataset
row.names(clean_Talb_mod) <- seq(nrow(clean_Talb_mod))


#Check for outliers by plotting
plot_Talb_mod <- clean_Talb_mod
coordinates(plot_Talb_mod) <- c("longitude", "latitude")
proj4string(plot_Talb_mod) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

plot(south_america)
plot(plot_Talb_mod, add=TRUE, col = "purple")

write.csv(clean_Talb_mod, "./Data/Cleaned_occurrences_by_species/Albescens_modified_postcleaning.csv", row.names = F)
