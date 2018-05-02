#Cleaning database occurrences - already some cleaning done

library(dplyr)
library(rgdal)
library(raster)
library(scrubr)
library(spatstat)
#Import points from file

points <- read.csv("./Data/Points_for_plotting.csv", stringsAsFactors = FALSE)

#Get objects for each species

Taeg_points <- points %>% 
  filter(species == "Tibouchina aegopogon")

Tasp_points <- points %>% 
  filter(species == "Tibouchina aspera")

Tbarb_points <- points %>% 
  filter(species == "Tibouchina barbigera")

Tmel_points <- points %>% 
  filter(species == "Tibouchina melastomoides")

#Step 3: Removing points with no environmental data
terrestrialAltitude <- raster("./Layers/terrestrialAltitude.asc")
south_america <- readOGR("./Data/joined.shp")

#crop terrestrialAltitude to south america
crop_terrAlt <- crop(terrestrialAltitude, extent(south_america))
extent(south_america)
extent(crop_terrAlt)

#Make sure only points from south america are included
Tmel_Extract <- extract(crop_terrAlt, sapply(Tmel_points[2:3], as.numeric))
Taeg_Extract <- extract(crop_terrAlt, sapply(Taeg_points[2:3], as.numeric))
Tasp_Extract <- extract(crop_terrAlt, sapply(Tasp_points[2:3], as.numeric))
Tbar_Extract <- extract(crop_terrAlt, sapply(Tbarb_points[2:3], as.numeric))

Tmel_Extract<- cbind(Tmel_points, Tmel_Extract)
Taeg_Extract<- cbind(Taeg_points, Taeg_Extract)
Tasp_Extract<- cbind(Tasp_points, Tasp_Extract)
Tbar_Extract<- cbind(Tbarb_points, Tbar_Extract)

clean_Tmel_Extract <- Tmel_Extract[complete.cases(Tmel_Extract[,4]),]
clean_Taeg_Extract <- Taeg_Extract[complete.cases(Taeg_Extract[,4]),]
clean_Tasp_Extract <- Tasp_Extract[complete.cases(Tasp_Extract[,4]),]
clean_Tbar_Extract <- Tbar_Extract[complete.cases(Tbar_Extract[,4]),]

#Step 4: Reduce points to resolution of environmental data
rasterResolution <- max(res(terrestrialAltitude))

while(min(nndist(clean_Tmel_Extract[,2:3])) < rasterResolution){
  nnD <- nndist(clean_Tmel_Extract[,2:3])
  clean_Tmel_Extract <- clean_Tmel_Extract[-(which(min(nnD) == nnD)[1]),]
}
row.names(clean_Tmel_Extract) <- seq(nrow(clean_Tmel_Extract))

while(min(nndist(clean_Taeg_Extract[,2:3])) < rasterResolution){
  nnD <- nndist(clean_Taeg_Extract[,2:3])
  clean_Taeg_Extract <- clean_Taeg_Extract[-(which(min(nnD) == nnD)[1]),]
}
row.names(clean_Taeg_Extract) <- seq(nrow(clean_Taeg_Extract))

while(min(nndist(clean_Tasp_Extract[,2:3])) < rasterResolution){
  nnD <- nndist(clean_Tasp_Extract[,2:3])
  clean_Tasp_Extract <- clean_Tasp_Extract[-(which(min(nnD) == nnD)[1]),]
}
row.names(clean_Tasp_Extract) <- seq(nrow(clean_Tasp_Extract))

while(min(nndist(clean_Tbar_Extract[,2:3])) < rasterResolution){
  nnD <- nndist(clean_Tbar_Extract[,2:3])
  clean_Tbar_Extract <- clean_Tbar_Extract[-(which(min(nnD) == nnD)[1]),]
}
row.names(clean_Tbar_Extract) <- seq(nrow(clean_Tbar_Extract))


#Saving your clean data
write.csv(clean_Tmel_Extract[,1:4], "./Data/clean_Tmel_PointsSPOCC.csv", row.names = F)
write.csv(clean_Taeg_Extract[,1:4], "./Data/clean_Taeg_PointsSPOCC.csv", row.names = F)
write.csv(clean_Tasp_Extract[,1:4], "./Data/clean_Tasp_PointsSPOCC.csv", row.names = F)
write.csv(clean_Tbar_Extract[,1:4], "./Data/clean_Tbar_PointsSPOCC.csv", row.names = F)


Taspera_points <- read.csv("./Data/Clean_Tasp_PointsSPOCC_no_outlier.csv", stringsAsFactors = FALSE)
Taspera_plot <- Taspera_points[,2:3]
coordinates(Taspera_points) <- c("Long", "Lat")
#Set crs
proj4string(Taspera_points) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
plot(Taspera)
plot(Taspera_points, add=TRUE, col = "red")

Taegopogon_points <- read.csv("./Data/Clean_Taeg_PointsSPOCC.csv", stringsAsFactors = FALSE)
coordinates(Taegopogon_points) <- c("Long", "Lat")
proj4string(Taegopogon_points) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
plot(Taegopogon, add=TRUE)
plot(Taegopogon_points, add=TRUE, col = "blue")


Tbarbigera_points <- read.csv("./Data/Clean_Tbar_PointsSPOCC.csv", stringsAsFactors = FALSE)
coordinates(Tbarbigera_points) <- c("Long", "Lat")
proj4string(Tbarbigera_points) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
plot(Tbarbigera, add=TRUE)
plot(Tbarbigera_points, add=TRUE, col = "green")


Tmelastomoides_points <- read.csv("./Data/Clean_Tmel_PointsSPOCC.csv", stringsAsFactors = FALSE)
coordinates(Tmelastomoides_points) <- c("Long", "Lat")
proj4string(Tmelastomoides_points) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
plot(Tmelastomoides, add=TRUE)
plot(Tmelastomoides_points, add=TRUE, col = "purple")

ordered_points <- Taspera_points %>% 
  arrange(Lat)
head(ordered_points)
ordered_points_removed <- ordered_points[-1,]
ordered_points_removed
write.csv(ordered_points_removed, "./Data/Clean_Tasp_PointsSPOCC_no_outlier.csv", row.names = F)
