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

Tverticillaris <- read.csv("./Data/Occurrence_records_by_species/Tverticillaris_unique.csv", stringsAsFactors = FALSE)
Tverticillaris <- Tverticillaris[,-c(2:5)]

colnames(Tverticillaris)[1] <- "name"

head(Tverticillaris)

Tverticillaris_clean <- coord_impossible(Tverticillaris)
nrow(Tverticillaris_clean)
#Remove points with no environmental data

#Make data spatial
Tverticillaris_spatial <- Tverticillaris_clean
coordinates(Tverticillaris_spatial) <- c("longitude", "latitude")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
proj4string(Tverticillaris_spatial) <- crs.geo


#Import environmental data
terrestrialAltitude <- raster("./Layers/terrestrialAltitude.asc")
south_america <- readOGR("./Layers/joined.shp")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
crs(terrestrialAltitude) <- crs.geo

crs(south_america)
crs(terrestrialAltitude) 
crs(Tverticillaris_spatial)

#Crop terrestrialAltitude to geographic area (South America)
crop_terrAlt <- crop(terrestrialAltitude, extent(south_america))

#Check that extents match
extent(south_america)
extent(crop_terrAlt)

#Extract points only from South America
Tverticillaris_extract <- extract(crop_terrAlt, Tverticillaris_spatial)

#Link complete dataset with extracted point yes/no
Tverticillaris_extract<- cbind(Tverticillaris_clean, Tverticillaris_extract)
Tverticillaris_extract

#Remove incomplete cases (those not in extracted dataset)
clean_Tverticillaris <- Tverticillaris_extract[complete.cases(Tverticillaris_extract[,4]),]

#Get resolution of raster of environmental data
rasterResolution <- max(res(terrestrialAltitude))
rasterResolution

#Reduce resolution
while(min(nndist(clean_Tverticillaris[,2:3])) < rasterResolution){
  nnD <- nndist(clean_Tverticillaris[,2:3])
  clean_Tverticillaris <- clean_Tverticillaris[-(which(min(nnD) == nnD)[1]),]
}

clean_Tverticillaris

#Set rownames of clean dataset
row.names(clean_Tverticillaris) <- seq(nrow(clean_Tverticillaris))


#Check for outliers by plotting
plot_Tverticillaris <- clean_Tverticillaris
coordinates(plot_Tverticillaris) <- c("longitude", "latitude")
proj4string(plot_Tverticillaris) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

plot(south_america)
plot(plot_Tverticillaris, add=TRUE, col = "purple")

ordered_points <- clean_Tverticillaris %>% 
  arrange(latitude)

ordered_points_removed <- ordered_points[-c(1:2),]


#Save the clean data
write.csv(ordered_points_removed, "./Data/Cleaned_occurrences_by_species/Tverticillaris_cleaned_occurrences.csv", row.names = F)
