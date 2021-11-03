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

Tbruniana <- read.csv("./Data/Occurrence_records_by_species/Tbruniana_unique.csv", stringsAsFactors = FALSE)
Tbruniana <- Tbruniana[,-c(2:4)]
#Remove duplicates
#Taegopogon <- unique(Taegopogon[,-2])
Tbruniana

#Set species epithet to name
colnames(Tbruniana)[1] <- "name"

head(Tbruniana)

Tbruniana_clean <- coord_impossible(Tbruniana)
nrow(Tbruniana_clean)
#Remove points with no environmental data

#Make data spatial
Tbruniana_spatial <- Tbruniana_clean
coordinates(Tbruniana_spatial) <- c("longitude", "latitude")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
proj4string(Tbruniana_spatial) <- crs.geo


#Import environmental data
terrestrialAltitude <- raster("./Layers/terrestrialAltitude.asc")
south_america <- readOGR("./Layers/joined.shp")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
crs(terrestrialAltitude) <- crs.geo

crs(south_america)
crs(terrestrialAltitude) 
crs(Tbruniana_spatial)

#Crop terrestrialAltitude to geographic area (South America)
crop_terrAlt <- crop(terrestrialAltitude, extent(south_america))

#Check that extents match
extent(south_america)
extent(crop_terrAlt)

#Extract points only from South America
Tbruniana_extract <- extract(crop_terrAlt, Tbruniana_spatial)

#Link complete dataset with extracted point yes/no
Tbruniana_extract<- cbind(Tbruniana_clean, Tbruniana_extract)
Tbruniana_extract

#Remove incomplete cases (those not in extracted dataset)
clean_Tbruniana <- Tbruniana_extract[complete.cases(Tbruniana_extract[,4]),]

#Get resolution of raster of environmental data
rasterResolution <- max(res(terrestrialAltitude))
rasterResolution

#Reduce resolution
while(min(nndist(clean_Tbruniana[,2:3])) < rasterResolution){
  nnD <- nndist(clean_Tbruniana[,2:3])
  clean_Tbruniana <- clean_Tbruniana[-(which(min(nnD) == nnD)[1]),]
}


#Set rownames of clean dataset
row.names(clean_Tbruniana) <- seq(nrow(clean_Tbruniana))


#Check for outliers by plotting
plot_Tbruniana <- clean_Tbruniana
coordinates(plot_Tbruniana) <- c("longitude", "latitude")
proj4string(plot_Tbruniana) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

plot(south_america)
plot(plot_Tbruniana, add=TRUE, col = "purple")

#To order points to remove specific outliers
ordered_points <- clean_Tbruniana %>% 
  arrange(latitude)
head(ordered_points)
tail(ordered_points)

ordered_points_removed <- ordered_points[-(nrow(clean_Tbruniana)),]

ordered_points_removed <- ordered_points_removed %>% 
  arrange(longitude)

ordered_points_removed

ordered_points_removed <- ordered_points_removed[-1,]


#Save the clean data
write.csv(ordered_points_removed[,1:3], "./Data/Cleaned_occurrences_by_species/Taegopogon_cleaned_occurrences.csv", row.names = F)
