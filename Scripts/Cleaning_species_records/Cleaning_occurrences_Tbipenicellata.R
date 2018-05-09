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

Tbipen <- read.csv("./Data/Occurrence_records_by_species/Tbipenicellata_unique.csv", stringsAsFactors = FALSE)
Tbipen <- Tbipen[,-c(2:5)]
#Remove duplicates
#Taegopogon <- unique(Taegopogon[,-2])
Tbipen

#Set species epithet to name
colnames(Tbipen)[1] <- "name"

head(Tbipen)

Tbipen_clean <- coord_impossible(Tbipen)
nrow(Tbipen_clean)
#Remove points with no environmental data

#Make data spatial
Tbipen_spatial <- Tbipen_clean
coordinates(Tbipen_spatial) <- c("longitude", "latitude")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
proj4string(Tbipen_spatial) <- crs.geo


#Import environmental data
terrestrialAltitude <- raster("./Layers/terrestrialAltitude.asc")
south_america <- readOGR("./Layers/joined.shp")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
crs(terrestrialAltitude) <- crs.geo

crs(south_america)
crs(terrestrialAltitude) 
crs(Tbipen_spatial)

#Crop terrestrialAltitude to geographic area (South America)
crop_terrAlt <- crop(terrestrialAltitude, extent(south_america))

#Check that extents match
extent(south_america)
extent(crop_terrAlt)

#Extract points only from South America
Tbipen_extract <- extract(crop_terrAlt, Tbipen_spatial)

#Link complete dataset with extracted point yes/no
Tbipen_extract<- cbind(Tbipen_clean, Tbipen_extract)
Tbipen_extract

#Remove incomplete cases (those not in extracted dataset)
clean_Tbipen <- Tbipen_extract[complete.cases(Tbipen_extract[,4]),]

#Get resolution of raster of environmental data
rasterResolution <- max(res(terrestrialAltitude))
rasterResolution

#Reduce resolution
while(min(nndist(clean_Tbipen[,2:3])) < rasterResolution){
  nnD <- nndist(clean_Tbipen[,2:3])
  clean_Tbipen <- clean_Tbipen[-(which(min(nnD) == nnD)[1]),]
}


#Set rownames of clean dataset
row.names(clean_Tbipen) <- seq(nrow(clean_Tbipen))


#Check for outliers by plotting
plot_Tbipen <- clean_Tbipen
coordinates(plot_Tbipen) <- c("longitude", "latitude")
proj4string(plot_Tbipen) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

plot(south_america)
plot(plot_Tbipen, add=TRUE, col = "purple")

#To order points to remove specific outliers
ordered_points <- clean_Tbipen %>% 
  arrange(latitude)
head(ordered_points)
tail(ordered_points)

ordered_points_removed <- ordered_points[-c(1:3),]

ordered_points_removed <- ordered_points_removed %>% 
  arrange(latitude)

ordered_points_removed

#Save the clean data
write.csv(ordered_points_removed[,1:3], "./Data/Cleaned_occurrences_by_species/Tbipenicillata_cleaned_occurrences.csv", row.names = F)
