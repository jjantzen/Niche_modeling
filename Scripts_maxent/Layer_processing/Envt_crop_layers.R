library(raster)
#Gets a list of .asc files
envtList <- list.files(path = "./Layers/Trimmed_worldclim/Barbigera/", pattern = ".asc", full.names = TRUE)

#Reads in .asc files as a raster stack
envtStack <- stack(envtList)
envtStack
#Defines projection of layers
crs(envtStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" 

SoilList <- list.files(path = "./Layers/Trimmed_soil/Verticillaris/", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files

soil <- SoilList[1]

soil_raster <- raster(soil)
soil_raster
#Set crs
crs(soil_raster) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" 

crop_EnvStack <- crop(envtStack, extent(soil_raster))

#compare Rasters (will give error if not matching??)
compareRaster(crop_EnvStack, soil_raster)
crop_EnvStack
#Write rasters
writeRaster(crop_EnvStack, filename = "./Layers/Trimmed_worldclim/Verticillaris/Cropped/Cropped_", 
            format = "ascii", bylayer = T, suffix=names(envtStack), NAFlag = "-9999", overwrite = F)

#writeRaster(crop_EnvStack, filename = paste0("./Layers/Trimmed_worldclim/Aegopogon/Cropped/Cropped_", names(envtStack)), format = "ascii", NAFlag = "-9999", overwrite = T)
            