library(raster); #For raster-based loading, calculations, and mapping
library(rgdal); #For reading the M polygon


#Loading a environmental raster stack
envtList <- list.files(path = "./WorldClimTerrestrial", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
envtStack <- stack(envtList); #Reads in .asc files as a raster stack
crs(envtStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers
plot(envtStack); #Plots all the layers of the raster stack object

#Loading soil layers
soilList <- list.files(path = "./Layers/Soil/Resolution/", pattern = ".asc", full.names=TRUE)
for (i in 5:length(soilList)){
  soil <- raster(soilList[i])
  name <- names(soil)
  writeRaster(soil, filename = paste0("./Layers/Soil/SoilGrid/", name), format = "ascii", overwrite=TRUE)
}
soil
soilRasters <- list.files(path = "./Layers/Soil/", pattern = ".asc", full.names=TRUE)
soilStack <- stack(soilRasters)
crs(soilStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
plot(soilStack)
extent(soilStack) <- extent(-180, 180, -60, 90)
extent(soilStack)
#Get M shapefile
Taegopogon <- readOGR(dsn = "./Data", layer = "Taeg_M"); #Reads in shapefile
Taspera <- readOGR(dsn = "./Data", layer = "Taspera_M_small")
Tbarbigera <- readOGR(dsn = "./Data", layer = "Tbar_M")
Tmelastomoides <- readOGR(dsn = "./Data", layer = "Tmel_M")

#plot(Taegopogon)
#Defines M projection as identical to envtStack
crs(Taegopogon) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
crs(Taspera) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
crs(Tbarbigera) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
crs(Tmelastomoides) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

#Saves all the layers in the stack, with "SyrmaticusSoemmerringii" as a prefix
Taspera_Training <- mask(envtStack, Taspera) #Masking the raster stack
writeRaster(Taspera_Training, filename = "./Layers/Taspera/Small_Taspera/Taspera", 
            format = "ascii", bylayer = T, suffix=names(envtStack), NAFlag = "-9999", overwrite = T)

#Masking your environmental variables to M
#Saves all the layers in the stack, with prefix

Taegopogon_Training <- mask(soilStack, Taegopogon) #Masking the raster stack
writeRaster(Taegopogon_Training, filename = "./Layers/Taegopogon/Taegopogon_Soil/Taegopogon", 
  format = "ascii", bylayer = T, suffix=names(soilStack), NAFlag = "-9999", overwrite = T)
    #Saves all the layers in the stack, with "SyrmaticusSoemmerringii" as a prefix
Taspera_Training <- mask(soilStack, Taspera) #Masking the raster stack
writeRaster(Taspera_Training, filename = "./Layers/Taspera/Taspera_Soil/Taspera", 
            format = "ascii", bylayer = T, suffix=names(soilStack), NAFlag = "-9999", overwrite = T)
#Saves all the layers in the stack
Tbarbigera_Training <- mask(soilStack, Tbarbigera) #Masking the raster stack
writeRaster(Tbarbigera_Training, filename = "./Layers/Tbarbigera/Tbarbigera_Soil/Tbarbigera", 
            format = "ascii", bylayer = T, suffix=names(soilStack), NAFlag = "-9999", overwrite = T)
#Saves all the layers in the stack, with "SyrmaticusSoemmerringii" as a prefix
Tmelastomoides_Training <- mask(soilStack, Tmelastomoides) #Masking the raster stack
writeRaster(Tmelastomoides_Training, filename = "./Layers/Tmelastomoides/Tmelastomoides_Soil/Tmelastomoides", 
            format = "ascii", bylayer = T, suffix=names(soilStack), NAFlag = "-9999", overwrite = T)


#Taeg_List <- list.files(path = "./Lab5/Taegopogon/", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
#Taegopogon_Training <- stack(Taeg_List); #Reads in .asc files as a raster stack
#crs(Taegopogon_Training) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers
#plot(Taegopogon_Training)
#Taeg_List


#Get layers for projecting onto
NW <- raster("./Lab3/croppedNW.asc")
plot(NW)
crs(NW)
envtList <- list.files(path = "./Lab5/WorldClimTerrestrial", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
envtList
envtStack <- stack(envtList); #Reads in .asc files as a raster stack
crs(envtStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers
crs(NW) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
NW_layers <- crop(envtStack, NW) #Masking the raster stack
writeRaster(NW_layers, filename = "./Lab5//NW/reduced_Tmelastomoides_Tmelastomoides", 
            format = "ascii", bylayer = T, suffix=names(envtStack), NAFlag = "-9999", overwrite = T)
extent(NW)
extent(envtStack)
