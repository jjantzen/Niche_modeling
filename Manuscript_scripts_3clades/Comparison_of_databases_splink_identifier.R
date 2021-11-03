#Comparison of with and without species link and with and without identifiers

#Load libraries
library(dplyr)
library(rgdal)
library(raster)
library(scrubr)
library(spatstat)

#read all three files in

splink_ident <- read.csv("./Data/3_clades/Tib_all_unique_with_identifier_sp_link.csv", header = TRUE, stringsAsFactors = FALSE)
ident <- read.csv("./Data/3_clades/Tib_all_unique_with_identifier.csv", header = TRUE, stringsAsFactors = FALSE)
original <- read.csv("./Data/3_clades/Tib_all_unique.csv", header = TRUE, stringsAsFactors = FALSE)

#compare number of rows
nrow(splink_ident)
nrow(ident)
nrow(original)

#Cleaning database occurrences (from clean database)
#Remove points outside geographic area
#Save cleaned data as csv files

#get rid of date columns
splink_ident <- splink_ident[,-c(2:4)]
ident <- ident[,-c(2:4)]
original <- original[,-c(2:4)]

#Set species epithet to name
colnames(splink_ident)[1] <- "name"
colnames(ident)[1] <- "name"
colnames(original)[1] <- "name"

splink_ident_clean <- coord_impossible(splink_ident)
ident_clean <- coord_impossible(ident)
original_clean <- coord_impossible(original)

nrow(splink_ident_clean)

#Remove points with no environmental data

#Make data spatial
splink_ident_spatial <- splink_ident_clean
coordinates(splink_ident_spatial) <- c("lon", "lat")

ident_spatial <- ident_clean
coordinates(ident_spatial) <- c("lon", "lat")

original_spatial <- original_clean
coordinates(original_spatial) <- c("lon", "lat")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
proj4string(splink_ident_spatial) <- crs.geo
proj4string(ident_spatial) <- crs.geo
proj4string(original_spatial) <- crs.geo


#Import environmental data
terrestrialAltitude <- raster("./Layers/terrestrialAltitude.asc")
south_america <- readOGR("./Layers/joined.shp")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
crs(terrestrialAltitude) <- crs.geo

crs(south_america)
crs(terrestrialAltitude) 
crs(original_spatial)

#Crop terrestrialAltitude to geographic area (South America)
crop_terrAlt <- crop(terrestrialAltitude, extent(south_america))

#Check that extents match
extent(south_america)
extent(crop_terrAlt)

#Save pruned terrAlt
#writeRaster(crop_terrAlt, "./Layers/cropped_terrAlt.asc")
#crop_terrAlt <- raster("./Layers/cropped_terrAlt.asc")
crs(crop_terrAlt) <- crs.geo

#Extract points only from South America
splink_ident_extract <- raster::extract(crop_terrAlt, splink_ident_spatial)
ident_extract <- raster::extract(crop_terrAlt, ident_spatial)
original_extract <- raster::extract(crop_terrAlt, original_spatial)

#Link complete dataset with extracted point yes/no
splink_ident_extract<- cbind(splink_ident_clean, splink_ident_extract)
ident_extract<- cbind(ident_clean, ident_extract)
original_extract<- cbind(original_clean, original_extract)

#Remove incomplete cases (those not in extracted dataset)
clean_splink_ident <- splink_ident_extract[complete.cases(splink_ident_extract[,4]),]
clean_ident <- ident_extract[complete.cases(ident_extract[,4]),]
clean_original <- original_extract[complete.cases(original_extract[,4]),]

#write cleaned points (not reduced)
write.csv(clean_splink_ident, "./Data/3_clades/Tib_all_cleaned_splink_ident.csv", row.names = FALSE)
write.csv(clean_ident, "./Data/3_clades/Tib_all_cleaned_ident.csv", row.names = FALSE)
write.csv(clean_original, "./Data/3_clades/Tib_all_cleaned_original.csv", row.names = FALSE)

nrow(clean_splink_ident)
nrow(clean_ident)
nrow(clean_original)
