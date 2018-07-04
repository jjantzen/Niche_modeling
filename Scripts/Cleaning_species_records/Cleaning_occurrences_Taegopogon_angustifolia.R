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

Tangustifolia <- read.csv("./Data/Occurrence_records_by_species/Taegopogon_angustifolia_unique.csv", stringsAsFactors = FALSE)
Tangustifolia <- Tangustifolia[,-c(1:6)]

Tangustifolia

#Set species epithet to name
Tangustifolia$name <- "angustifolia"

head(Tangustifolia)

Tangustifolia_clean <- coord_impossible(Tangustifolia)
nrow(Tangustifolia_clean)
#Remove points with no environmental data

#Make data spatial
Tangustifolia_spatial <- Tangustifolia_clean
coordinates(Tangustifolia_spatial) <- c("longitude", "latitude")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
proj4string(Tangustifolia_spatial) <- crs.geo


#Import environmental data
terrestrialAltitude <- raster("./Layers/terrestrialAltitude.asc")
south_america <- readOGR("./Layers/joined.shp")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
crs(terrestrialAltitude) <- crs.geo

crs(south_america)
crs(terrestrialAltitude) 
crs(Tangustifolia_spatial)

#Crop terrestrialAltitude to geographic area (South America)
crop_terrAlt <- crop(terrestrialAltitude, extent(south_america))

#Check that extents match
extent(south_america)
extent(crop_terrAlt)

#Extract points only from South America
Tangustifolia_extract <- extract(crop_terrAlt, Tangustifolia_spatial)

#Link complete dataset with extracted point yes/no
Tangustifolia_extract<- cbind(Tangustifolia_clean, Tangustifolia_extract)
Tangustifolia_extract

#Remove incomplete cases (those not in extracted dataset)
clean_Tangustifolia <- Tangustifolia_extract[complete.cases(Tangustifolia_extract[,4]),]

#Get resolution of raster of environmental data
rasterResolution <- max(res(terrestrialAltitude))
rasterResolution

#Reduce resolution
while(min(nndist(clean_Tangustifolia[,1:2])) < rasterResolution){
  nnD <- nndist(clean_Tangustifolia[,1:2])
  clean_Tangustifolia <- clean_Tangustifolia[-(which(min(nnD) == nnD)[1]),]
}

clean_Tangustifolia

#Set rownames of clean dataset
row.names(clean_Tangustifolia) <- seq(nrow(clean_Tangustifolia))


#Check for outliers by plotting
plot_Tangustifolia <- clean_Tangustifolia
coordinates(plot_Tangustifolia) <- c("longitude", "latitude")
proj4string(plot_Tangustifolia) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

plot(south_america)
plot(plot_Tangustifolia, add=TRUE, col = "purple")

#To order points to remove specific outliers
ordered_points <- clean_Tangustifolia %>% 
  arrange(latitude)
head(ordered_points)
tail(ordered_points)

ordered_points_removed <- ordered_points[-1,]
ordered_points_removed <- ordered_points_removed[-(nrow(ordered_points_removed)),]
ordered_points_removed

coordinates(ordered_points_removed) <- c("longitude", "latitude")
proj4string(ordered_points_removed) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

plot(south_america)
plot(ordered_points_removed, add=TRUE, col = "purple")

#Save the clean data
write.csv(ordered_points_removed, "./Data/Cleaned_occurrences_by_species/Taegopogon_angustifolia_cleaned_occurrences.csv", row.names = F)


#### For after adding field collections in (and NYBG specimens)
mod_Tangust <- read.csv("./Data/Cleaned_occurrences_by_species/Taegopogon_angustifolia_cleaned_occurrences_MOD.csv", stringsAsFactors = FALSE)

mod_Tangust
mod_Tangust_clean <- coord_impossible(mod_Tangust)
mod_Tangust
mod_Tangust_clean

mod_Tangust_clean_spatial <- mod_Tangust_clean
coordinates(mod_Tangust_clean_spatial) <- c("longitude", "latitude")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
proj4string(mod_Tangust_clean_spatial) <- crs.geo

#Extract points only from South America
Tangust_mod_extract <- extract(crop_terrAlt, mod_Tangust_clean_spatial)

#Link complete dataset with extracted point yes/no
Tangust_mod_extract<- cbind(mod_Tangust_clean, Tangust_mod_extract)
Tangust_mod_extract

#Remove incomplete cases (those not in extracted dataset)
clean_Tangust_mod <- Tangust_mod_extract[complete.cases(Tangust_mod_extract[,4]),]
clean_Tangust_mod
#Get resolution of raster of environmental data
rasterResolution <- max(res(terrestrialAltitude))
rasterResolution

#Reduce resolution
while(min(nndist(clean_Tangust_mod[,2:3])) < rasterResolution){
  nnD <- nndist(clean_Tangust_mod[,2:3])
  clean_Tangust_mod <- clean_Tangust_mod[-(which(min(nnD) == nnD)[1]),]
}

clean_Tangust_mod
#####here 
#Set rownames of clean dataset
row.names(clean_Tangust_mod) <- seq(nrow(clean_Tangust_mod))

clean_Tangust_mod
#Check for outliers by plotting
plot_Tangust_mod <- clean_Tangust_mod
coordinates(plot_Tangust_mod) <- c("longitude", "latitude")
proj4string(plot_Tangust_mod) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

plot(south_america)
plot(plot_Tangust_mod, add=TRUE, col = "purple")

ordered_points <- clean_Tangust_mod %>% 
  arrange(latitude)

ordered_points

ordered_points_removed <- ordered_points[-(nrow(ordered_points)),]

ordered_points_removed

write.csv(ordered_points_removed, "./Data/Cleaned_occurrences_by_species/Aegopogon_angustifolia_modified_postcleaning.csv", row.names = F)
