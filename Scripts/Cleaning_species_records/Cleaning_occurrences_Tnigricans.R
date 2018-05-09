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

Tnigricans <- read.csv("./Data/Occurrence_records_by_species/Tnigricans_unique.csv", stringsAsFactors = FALSE)
Tnigricans <- Tnigricans[,-c(2:5)]

colnames(Tnigricans)[1] <- "name"

head(Tnigricans)

Tnigricans_clean <- coord_impossible(Tnigricans)
nrow(Tnigricans_clean)
#Remove points with no environmental data

#Make data spatial
Tnigricans_spatial <- Tnigricans_clean
coordinates(Tnigricans_spatial) <- c("longitude", "latitude")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
proj4string(Tnigricans_spatial) <- crs.geo


#Import environmental data
terrestrialAltitude <- raster("./Layers/terrestrialAltitude.asc")
south_america <- readOGR("./Layers/joined.shp")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
crs(terrestrialAltitude) <- crs.geo

crs(south_america)
crs(terrestrialAltitude) 
crs(Tnigricans_spatial)

#Crop terrestrialAltitude to geographic area (South America)
crop_terrAlt <- crop(terrestrialAltitude, extent(south_america))

#Check that extents match
extent(south_america)
extent(crop_terrAlt)

#Extract points only from South America
Tnigricans_extract <- extract(crop_terrAlt, Tnigricans_spatial)

#Link complete dataset with extracted point yes/no
Tnigricans_extract<- cbind(Tnigricans_clean, Tnigricans_extract)
Tnigricans_extract

#Remove incomplete cases (those not in extracted dataset)
clean_Tnigricans <- Tnigricans_extract[complete.cases(Tnigricans_extract[,4]),]

#Get resolution of raster of environmental data
rasterResolution <- max(res(terrestrialAltitude))
rasterResolution

#Reduce resolution
while(min(nndist(clean_Tnigricans[,2:3])) < rasterResolution){
  nnD <- nndist(clean_Tnigricans[,2:3])
  clean_Tnigricans <- clean_Tnigricans[-(which(min(nnD) == nnD)[1]),]
}

clean_Tnigricans

#Set rownames of clean dataset
row.names(clean_Tnigricans) <- seq(nrow(clean_Tnigricans))


#Check for outliers by plotting
plot_Tnigricans <- clean_Tnigricans
coordinates(plot_Tnigricans) <- c("longitude", "latitude")
proj4string(plot_Tnigricans) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

plot(south_america)
plot(plot_Tnigricans, add=TRUE, col = "purple")

ordered_points <- clean_Tnigricans %>% 
  arrange(latitude)

ordered_points_removed <- ordered_points[-5,]


#Save the clean data
write.csv(ordered_points_removed, "./Data/Cleaned_occurrences_by_species/Tnigricans_cleaned_occurrences.csv", row.names = F)
