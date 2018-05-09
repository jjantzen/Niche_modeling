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

Trosanae <- read.csv("./Data/Occurrence_records_by_species/Trosanae_unique.csv", stringsAsFactors = FALSE)
Trosanae <- Trosanae[,-c(2:5)]

colnames(Trosanae)[1] <- "name"

head(Trosanae)

Trosanae_clean <- coord_impossible(Trosanae)
nrow(Trosanae_clean)
#Remove points with no environmental data

#Make data spatial
Trosanae_spatial <- Trosanae_clean
coordinates(Trosanae_spatial) <- c("longitude", "latitude")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
proj4string(Trosanae_spatial) <- crs.geo


#Import environmental data
terrestrialAltitude <- raster("./Layers/terrestrialAltitude.asc")
south_america <- readOGR("./Layers/joined.shp")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
crs(terrestrialAltitude) <- crs.geo

crs(south_america)
crs(terrestrialAltitude) 
crs(Trosanae_spatial)

#Crop terrestrialAltitude to geographic area (South America)
crop_terrAlt <- crop(terrestrialAltitude, extent(south_america))

#Check that extents match
extent(south_america)
extent(crop_terrAlt)

#Extract points only from South America
Trosanae_extract <- extract(crop_terrAlt, Trosanae_spatial)

#Link complete dataset with extracted point yes/no
Trosanae_extract<- cbind(Trosanae_clean, Trosanae_extract)
Trosanae_extract

#Remove incomplete cases (those not in extracted dataset)
clean_Trosanae <- Trosanae_extract[complete.cases(Trosanae_extract[,4]),]

#Get resolution of raster of environmental data
rasterResolution <- max(res(terrestrialAltitude))
rasterResolution

#Reduce resolution
while(min(nndist(clean_Trosanae[,2:3])) < rasterResolution){
  nnD <- nndist(clean_Trosanae[,2:3])
  clean_Trosanae <- clean_Trosanae[-(which(min(nnD) == nnD)[1]),]
}

clean_Trosanae

#Set rownames of clean dataset
row.names(clean_Trosanae) <- seq(nrow(clean_Trosanae))


#Check for outliers by plotting
plot_Trosanae <- clean_Trosanae
coordinates(plot_Trosanae) <- c("longitude", "latitude")
proj4string(plot_Trosanae) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

plot(south_america)
plot(plot_Trosanae, add=TRUE, col = "purple")

#Save the clean data
write.csv(clean_Trosanae, "./Data/Cleaned_occurrences_by_species/Trosanae_cleaned_occurrences.csv", row.names = F)
