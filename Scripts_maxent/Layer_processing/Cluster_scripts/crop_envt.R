#Crop environmental variables to match extent of soil variables

library(raster)

#####Generic script for batch

#Get list of soil layers
SoilList <- list.files(path = "../../Trimmed_soil_layers/Aegopogon/", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files

soil <- SoilList[1]
#Stack soil layers
#SoilStack <- stack(SoilList)
soil_raster <- raster(soil)
#Set crs
crs(soil_raster) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" 

#Get list of envt layers
EnvList <- list.files(path = ".", pattern = ".asc", full.names = TRUE)

#Stack envt layers
EnvStack <- stack(EnvList)

#Set crs
crs(EnvStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" 

#Crop envt extent to match soil extents
crop_EnvStack <- crop(EnvStack, extent(soil_raster))

#compare Rasters (will give error if not matching??)
compareRaster(EnvStack, soil_raster)

#Write rasters
writeRaster(crop_EnvStack, filename = paste0("./Cropped/",names(EnvStack)),# format = "ascii", NAFlag = "-9999", overwrite = T)

################################

#
#####For specific species
#
##Get all environmental and soil layers loaded
#
##Get list of soil layers
#AegSoilList <- list.files(path = "./Layers/Trimmed_soil/Aegopogon", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
#
##Stack soil layers
#AegSoilStack <- stack(AegSoilList)
#
##Set crs
#crs(AegSoilStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" 
#
##Get list of envt layers
#AegEnvList <- list.files(path = "./Layers/Trimmed_worldclim/Aegopogon", pattern = ".asc", full.names = TRUE)
#
##Stack envt layers
#AegEnvStack <- stack(AegEnvList)
#
##Set crs
#crs(AegEnvStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" 
#
##Crop envt extent to match soil extents
#crop_AegEnvStack <- crop(AegEnvStack, extent(AegSoilStack))
#
##compare Rasters (will give error if not matching??)
#compareRaster(AegEnvStack, AegSoilStack)
#
##Write rasters
#writeRaster(crop_AegEnvStack, filename = paste0("./Trimmed_env_layers/Cropped/Aegopogon",names(AegEnvStack)),# format = "ascii", NAFlag = "-9999", overwrite = T)
#
#
###Albescens
##Get list of soil layers
#AlbSoilList <- list.files(path = "./Layers/Trimmed_soil/Albescens", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
#
##Stack soil layers
#AlbSoilStack <- stack(AlbSoilList)
#
##Set crs
#crs(AlbSoilStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" 
#
##Get list of envt layers
#AlbEnvList <- list.files(path = "./Layers/Trimmed_worldclim/Albescens", pattern = ".asc", full.names = TRUE)
#
##Stack envt layers
#AlbEnvStack <- stack(AlbEnvList)
#
##Set crs
#crs(AlbEnvStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" 
#
##Crop envt extent to match soil extents
#crop_AlbEnvStack <- crop(AlbEnvStack, extent(AlbSoilStack))
#
##compare Rasters (will give error if not matching??)
#compareRaster(AlbEnvStack, AlbSoilStack)
#
##Write rasters
#writeRaster(crop_AlbEnvStack, filename = paste0("./Trimmed_env_layers/Cropped/Albescens",names(AlbEnvStack)),# format = "ascii", NAFlag = "-9999", overwrite = T)
#
####Angustifolia
##Get list of soil layers
#AngustSoilList <- list.files(path = "./Layers/Trimmed_soil/Angustifolia", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
#
##Stack soil layers
#AngustSoilStack <- stack(AngustSoilList)
#
##Set crs
#crs(AngustSoilStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" 
#
##Get list of envt layers
#AngustEnvList <- list.files(path = "./Layers/Trimmed_worldclim/Angustifolia", pattern = ".asc", full.names = TRUE)
#
##Stack envt layers
#AngustEnvStack <- stack(AngustEnvList)
#
##Set crs
#crs(AngustEnvStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" 
#
##Crop envt extent to match soil extents
#crop_AngustEnvStack <- crop(AngustEnvStack, extent(AngustSoilStack))
#
##compare Rasters (will give error if not matching??)
#compareRaster(AngustEnvStack, AngustSoilStack)
#
##Write rasters
#writeRaster(crop_AngustEnvStack, filename = paste0("./Trimmed_env_layers/Cropped/Angustifolia",names(AngustEnvStack)),# format = "ascii", NAFlag = "-9999", overwrite = T)
#
#########Aspera
##Get list of soil layers
#AspSoilList <- list.files(path = "./Layers/Trimmed_soil/Aspera", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
#
##Stack soil layers
#AspSoilStack <- stack(AspSoilList)
#
##Set crs
#crs(AspSoilStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" 
#
##Get list of envt layers
#AspEnvList <- list.files(path = "./Layers/Trimmed_worldclim/Aspera", pattern = ".asc", full.names = TRUE)
#
##Stack envt layers
#AspEnvStack <- stack(AspEnvList)
#
##Set crs
#crs(AspEnvStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" 
#
##Crop envt extent to match soil extents
#crop_AspEnvStack <- crop(AspEnvStack, extent(AspSoilStack))
#
##compare Rasters (will give error if not matching??)
#compareRaster(AspEnvStack, AspSoilStack)
#
##Write rasters
#writeRaster(crop_AspEnvStack, filename = paste0("./Trimmed_env_layers/Cropped/Aspera",names(AspEnvStack)),# format = "ascii", NAFlag = "-9999", overwrite = T)
#
########Barbigera
##Get list of soil layers
#BarSoilList <- list.files(path = "./Layers/Trimmed_soil/Barbigera", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
#
##Stack soil layers
#BarSoilStack <- stack(BarSoilList)
#
##Set crs
#crs(BarSoilStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" 
#
##Get list of envt layers
#BarEnvList <- list.files(path = "./Layers/Trimmed_worldclim/Barbigera", pattern = ".asc", full.names = TRUE)
#
##Stack envt layers
#BarEnvStack <- stack(BarEnvList)
#
##Set crs
#crs(BarEnvStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" 
#
##Crop envt extent to match soil extents
#crop_BarEnvStack <- crop(BarEnvStack, extent(BarSoilStack))
#
##compare Rasters (will give error if not matching??)
#compareRaster(BarEnvStack, BarSoilStack)
#
##Write rasters
#writeRaster(crop_BarEnvStack, filename = paste0("./Trimmed_env_layers/Cropped/Barbigera",names(BarEnvStack)),# format = "ascii", NAFlag = "-9999", overwrite = T)
#
#####Bipenicillata
##Get list of soil layers
#BipenSoilList <- list.files(path = "./Layers/Trimmed_soil/Bipenicillata", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
#
##Stack soil layers
#BipenSoilStack <- stack(BipenSoilList)
#
##Set crs
#crs(BipenSoilStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" 
#
##Get list of envt layers
#BipenEnvList <- list.files(path = "./Layers/Trimmed_worldclim/Bipenicillata", pattern = ".asc", full.names = TRUE)
#
##Stack envt layers
#BipenEnvStack <- stack(BipenEnvList)
#
##Set crs
#crs(BipenEnvStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" 
#
##Crop envt extent to match soil extents
#crop_BipenEnvStack <- crop(BipenEnvStack, extent(BipenSoilStack))
#
##compare Rasters (will give error if not matching??)
#compareRaster(BipenEnvStack, BipenSoilStack)
#
##Write rasters
#writeRaster(crop_BipenEnvStack, filename = paste0("./Trimmed_env_layers/Cropped/Bipenicillata",names(BipenEnvStack)),# format = "ascii", NAFlag = "-9999", overwrite = T)
#
#
######Dissitiflora
##Get list of soil layers
#DissSoilList <- list.files(path = "./Layers/Trimmed_soil/Dissitiflora", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
#
##Stack soil layers
#DissSoilStack <- stack(DissSoilList)
#
##Set crs
#crs(DissSoilStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" 
#
##Get list of envt layers
#DissEnvList <- list.files(path = "./Layers/Trimmed_worldclim/Dissitiflora", pattern = ".asc", full.names = TRUE)
#
##Stack envt layers
#DissEnvStack <- stack(DissEnvList)
#
##Set crs
#crs(DissEnvStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" 
#
##Crop envt extent to match soil extents
#crop_DissEnvStack <- crop(DissEnvStack, extent(DissSoilStack))
#
##compare Rasters (will give error if not matching??)
#compareRaster(DissEnvStack, DissSoilStack)
#
##Write rasters
#writeRaster(crop_DissEnvStack, filename = paste0("./Trimmed_env_layers/Cropped/Dissitiflora",names(DissEnvStack)),# format = "ascii", NAFlag = "-9999", overwrite = T)
#
#####Duidae
##Get list of soil layers
#DuidSoilList <- list.files(path = "./Layers/Trimmed_soil/Duidae", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
#
##Stack soil layers
#DuidSoilStack <- stack(DuidSoilList)
#
##Set crs
#crs(DuidSoilStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" 
#
##Get list of envt layers
#DuidEnvList <- list.files(path = "./Layers/Trimmed_worldclim/Duidae", pattern = ".asc", full.names = TRUE)
#
##Stack envt layers
#DuidEnvStack <- stack(DuidEnvList)
#
##Set crs
#crs(DuidEnvStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" 
#
##Crop envt extent to match soil extents
#crop_DuidEnvStack <- crop(DuidEnvStack, extent(DuidSoilStack))
#
##compare Rasters (will give error if not matching??)
#compareRaster(DuidEnvStack, DuidSoilStack)
#
##Write rasters
#writeRaster(crop_DuidEnvStack, filename = paste0("./Trimmed_env_layers/Cropped/Duidae",names(DuidEnvStack)),# format = "ascii", NAFlag = "-9999", overwrite = T)