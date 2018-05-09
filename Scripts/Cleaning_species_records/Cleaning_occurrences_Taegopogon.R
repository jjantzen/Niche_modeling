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

Taegopogon <- read.csv("./Data/Occurrence_records_by_species/Taegopogon_unique.csv", stringsAsFactors = FALSE)
Taegopogon <- Taegopogon[,-c(2:4)]
#Remove duplicates
#Taegopogon <- unique(Taegopogon[,-2])
Taegopogon

#Set species epithet to name
colnames(Taegopogon)[1] <- "name"

head(Taegopogon)

Taegopogon_clean <- coord_impossible(Taegopogon)
nrow(Taegopogon_clean)
#Remove points with no environmental data

#Make data spatial
Taegopogon_spatial <- Taegopogon_clean
coordinates(Taegopogon_spatial) <- c("longitude", "latitude")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
proj4string(Taegopogon_spatial) <- crs.geo


#Import environmental data
terrestrialAltitude <- raster("./Layers/terrestrialAltitude.asc")
south_america <- readOGR("./Layers/joined.shp")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
crs(terrestrialAltitude) <- crs.geo

crs(south_america)
crs(terrestrialAltitude) 
crs(Taegopogon_spatial)

#Crop terrestrialAltitude to geographic area (South America)
crop_terrAlt <- crop(terrestrialAltitude, extent(south_america))

#Check that extents match
extent(south_america)
extent(crop_terrAlt)

#Extract points only from South America
Taegopogon_extract <- extract(crop_terrAlt, Taegopogon_spatial)

#Link complete dataset with extracted point yes/no
Taegopogon_extract<- cbind(Taegopogon_clean, Taegopogon_extract)
Taegopogon_extract

#Remove incomplete cases (those not in extracted dataset)
clean_Taegopogon <- Taegopogon_extract[complete.cases(Taegopogon_extract[,4]),]

#Get resolution of raster of environmental data
rasterResolution <- max(res(terrestrialAltitude))
rasterResolution

#Reduce resolution
while(min(nndist(clean_Taegopogon[,2:3])) < rasterResolution){
  nnD <- nndist(clean_Taegopogon[,2:3])
  clean_Taegopogon <- clean_Taegopogon[-(which(min(nnD) == nnD)[1]),]
}


#Set rownames of clean dataset
row.names(clean_Taegopogon) <- seq(nrow(clean_Taegopogon))


#Check for outliers by plotting
plot_Taegopogon <- clean_Taegopogon
coordinates(ordered_points_removed) <- c("longitude", "latitude")
proj4string(ordered_points_removed) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

plot(south_america)
plot(ordered_points_removed, add=TRUE, col = "purple")

#To order points to remove specific outliers
ordered_points <- clean_Taegopogon %>% 
  arrange(latitude)
head(ordered_points)
tail(ordered_points)

ordered_points_removed <- ordered_points[-(nrow(clean_Taegopogon)),]

ordered_points_removed <- ordered_points_removed %>% 
  arrange(longitude)

ordered_points_removed

ordered_points_removed <- ordered_points_removed[-1,]


#Save the clean data
write.csv(ordered_points_removed[,1:3], "./Data/Cleaned_occurrences_by_species/Taegopogon_cleaned_occurrences.csv", row.names = F)
