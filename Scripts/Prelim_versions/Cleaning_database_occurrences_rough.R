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

#Import points from database

#Write query script

Taegopogon <- read.csv("./Data/Occurrence_records_by_species/Taegopogon_unique.csv", stringsAsFactors = FALSE)

#Remove duplicates
unique(Taegopogon)




#Remove points with no environmental data

#Import environmental data
terrestrialAltitude <- raster("./Layers/terrestrialAltitude.asc")
south_america <- readOGR("./Data/joined.shp")

#Crop terrestrialAltitude to geographic area (South America)
crop_terrAlt <- crop(terrestrialAltitude, extent(south_america))

#Check that extents match
extent(south_america)
extent(crop_terrAlt)

#Extract points only from South America
Tmel_Extract <- extract(crop_terrAlt, sapply(Tmel_points[2:3], as.numeric))

#Link complete dataset with extracted point yes/no
Tmel_Extract<- cbind(Tmel_points, Tmel_Extract)

#Remove incomplete cases (those not in extracted dataset)
clean_Tmel_Extract <- Tmel_Extract[complete.cases(Tmel_Extract[,4]),]


#Reduce points to resolution of environmental data

#Get resolution of raster of environmental data
rasterResolution <- max(res(terrestrialAltitude))

#Reduce resolution
while(min(nndist(clean_Tmel_Extract[,2:3])) < rasterResolution){
  nnD <- nndist(clean_Tmel_Extract[,2:3])
  clean_Tmel_Extract <- clean_Tmel_Extract[-(which(min(nnD) == nnD)[1]),]
}

#Set rownames of clean dataset
row.names(clean_Tmel_Extract) <- seq(nrow(clean_Tmel_Extract))


#Check for outliers by plotting
coordinates(clean_Tmel_Extract) <- c("Long", "Lat")
proj4string(clean_Tmel_Extract) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
plot(south_america, add=TRUE)
plot(clean_Tmel_Extract, add=TRUE, col = "purple")

#To order points to remove specific outliers
ordered_points <- clean_Tmel_Extract %>% 
  arrange(Lat)
head(ordered_points)
ordered_points_removed <- ordered_points[-1,]


#Save the clean data
write.csv(clean_Tmel_Extract[,1:4], "./Data/clean_Tmel_PointsSPOCC.csv", row.names = F)
