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

Tmelastomoides <- read.csv("./Data/Occurrence_records_by_species/Tmelastomoides_unique.csv", stringsAsFactors = FALSE)
Tmelastomoides <- Tmelastomoides[,-c(2:5)]

colnames(Tmelastomoides)[1] <- "name"

head(Tmelastomoides)

Tmelastomoides_clean <- coord_impossible(Tmelastomoides)
nrow(Tmelastomoides_clean)
#Remove points with no environmental data

#Make data spatial
Tmelastomoides_spatial <- Tmelastomoides_clean
coordinates(Tmelastomoides_spatial) <- c("longitude", "latitude")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
proj4string(Tmelastomoides_spatial) <- crs.geo


#Import environmental data
terrestrialAltitude <- raster("./Layers/terrestrialAltitude.asc")
south_america <- readOGR("./Layers/joined.shp")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
crs(terrestrialAltitude) <- crs.geo

crs(south_america)
crs(terrestrialAltitude) 
crs(Tmelastomoides_spatial)

#Crop terrestrialAltitude to geographic area (South America)
crop_terrAlt <- crop(terrestrialAltitude, extent(south_america))

#Check that extents match
extent(south_america)
extent(crop_terrAlt)

#Extract points only from South America
Tmelastomoides_extract <- extract(crop_terrAlt, Tmelastomoides_spatial)

#Link complete dataset with extracted point yes/no
Tmelastomoides_extract<- cbind(Tmelastomoides_clean, Tmelastomoides_extract)
Tmelastomoides_extract

#Remove incomplete cases (those not in extracted dataset)
clean_Tmelastomoides <- Tmelastomoides_extract[complete.cases(Tmelastomoides_extract[,4]),]

#Get resolution of raster of environmental data
rasterResolution <- max(res(terrestrialAltitude))
rasterResolution

#Reduce resolution
while(min(nndist(clean_Tmelastomoides[,2:3])) < rasterResolution){
  nnD <- nndist(clean_Tmelastomoides[,2:3])
  clean_Tmelastomoides <- clean_Tmelastomoides[-(which(min(nnD) == nnD)[1]),]
}

clean_Tmelastomoides

#Set rownames of clean dataset
row.names(clean_Tmelastomoides) <- seq(nrow(clean_Tmelastomoides))


#Check for outliers by plotting
plot_Tmelastomoides <- clean_Tmelastomoides
coordinates(plot_Tmelastomoides) <- c("longitude", "latitude")
proj4string(plot_Tmelastomoides) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

plot(south_america)
plot(plot_Tmelastomoides, add=TRUE, col = "purple")

#Save the clean data
write.csv(clean_Tmelastomoides, "./Data/Cleaned_occurrences_by_species/Tmelastomoides_cleaned_occurrences.csv", row.names = F)
