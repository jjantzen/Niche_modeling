#Cleaning database occurrences (from clean database)

#Remove points outside geographic area

#Save cleaned data as csv files


#Load libraries
library(dplyr)
library(rgdal)
library(raster)
library(scrubr)
library(spatstat)

#Import points from file

points <- read.csv("./Data/3_clades/Tib_all_unique.csv", stringsAsFactors = FALSE)
points <- points[,-c(2:4)]

#Set species epithet to name
colnames(points)[1] <- "name"

head(points)

points_clean <- coord_impossible(points)
nrow(points_clean)
#Remove points with no environmental data

#Make data spatial
points_spatial <- points_clean
coordinates(points_spatial) <- c("lon", "lat")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
proj4string(points_spatial) <- crs.geo


#Import environmental data
terrestrialAltitude <- raster("./Layers/terrestrialAltitude.asc")
south_america <- readOGR("./Layers/joined.shp")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
crs(terrestrialAltitude) <- crs.geo

crs(south_america)
crs(terrestrialAltitude) 
crs(points_spatial)

#Crop terrestrialAltitude to geographic area (South America)
#crop_terrAlt <- crop(terrestrialAltitude, extent(south_america))

#Check that extents match
extent(south_america)
extent(crop_terrAlt)

#Save pruned terrAlt
#writeRaster(crop_terrAlt, "./Layers/cropped_terrAlt.asc")
crop_terrAlt <- raster("./Layers/cropped_terrAlt.asc")
crs(crop_terrAlt) <- crs.geo

#Extract points only from South America
points_extract <- raster::extract(crop_terrAlt, points_spatial)

#Link complete dataset with extracted point yes/no
points_extract<- cbind(points_clean, points_extract)
points_extract

#Remove incomplete cases (those not in extracted dataset)
clean_points <- points_extract[complete.cases(points_extract[,4]),]

#write cleaned points (not reduced)
write.csv(clean_points, "./Data/3_clades/Tib_all_cleaned.csv", row.names = FALSE)

