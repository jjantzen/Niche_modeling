#Step xx
#After creating M shapefiles in QGIS for training region
#Trimming environmental layers to M shapes/training regions

library(raster); #For raster-based loading, calculations, and mapping
library(rgdal); #For reading the M polygon
library(sf)

#Loading a environmental raster stack

#Gets a list of .asc files
envtList <- list.files(path = "./Layers/Trimmed_worldclim/Aegopogon/", pattern = ".asc", full.names = TRUE)

#Reads in .asc files as a raster stack
envtStack <- stack(envtList)
envtStack
#Defines projection of layers
crs(envtStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" 

#Get M shapefile

#Reads in shapefile
Aegopogon <- readOGR(dsn = "./Layers/Training_regions", layer = "Aegopogon_training_region")
Angustifolia <- readOGR(dsn = "./Layers/Training_regions", layer = "Angustifolia_training_region")
Albescens <- readOGR(dsn = "./Layers/Training_regions", layer = "Albescens_training_region")
Aspera <- readOGR(dsn = "./Layers/Training_regions", layer = "Aspera_training_region")
Barbigera <- readOGR(dsn = "./Layers/Training_regions", layer = "Barbigera_training_region")
Fraterna <- readOGR(dsn = "./Layers/Training_regions", layer = "Fraterna_training_region")
Melastomoides <- readOGR(dsn = "./Layers/Training_regions", layer = "Melastomoides_training_region")
Bipenicillata <- readOGR(dsn = "./Layers/Training_regions", layer = "Bipenicillata_training_region")
Papyrus <- readOGR(dsn = "./Layers/Training_regions", layer = "Papyrus_training_region")
Dissitiflora <- readOGR(dsn = "./Layers/Training_regions", layer = "Dissitiflora_training_region")
Llanorum <- readOGR(dsn = "./Layers/Training_regions", layer = "Llanorum_training_region_combined")
Duidae <- readOGR(dsn = "./Layers/Training_regions", layer = "Duidae_training_region")
Nigricans <- readOGR(dsn = "./Layers/Training_regions", layer = "Nigricans_training_region")
Verticillaris <- readOGR(dsn = "./Layers/Training_regions", layer = "Verticillaris_training_region")

#Defines M projection as identical to envtStack
crs.geo <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

crs(Aegopogon) <- crs.geo
crs(Aspera) <- crs.geo
crs(Angustifolia) <- crs.geo
crs(Albescens) <- crs.geo
crs(Fraterna) <- crs.geo
crs(Barbigera) <- crs.geo
crs(Melastomoides) <- crs.geo
crs(Bipenicillata) <- crs.geo
crs(Llanorum) <- crs.geo
crs(Nigricans) <- crs.geo
crs(Papyrus) <- crs.geo
crs(Dissitiflora) <- crs.geo
crs(Duidae) <- crs.geo
crs(Verticillaris) <- crs.geo

#Mask raster to training region
Aspera_env <- mask(envtStack, Aspera)
str(Aspera_env)
writeRaster(Aspera_env, filename = "./Layers/Trimmed_worldclim/Aspera/Aspera", 
            format = "ascii", bylayer = T, suffix=names(envtStack), NAFlag = "-9999", overwrite = T)

Aegopogon_env <- mask(envtStack, Aegopogon)
writeRaster(Aegopogon_env, filename = "./Layers/Trimmed_worldclim/Aegopogon/Aegopogon", 
            format = "ascii", bylayer = T, suffix=names(envtStack), NAFlag = "-9999", overwrite = T)

Angustifolia_env <- mask(envtStack, Angustifolia)
writeRaster(Angustifolia_env, filename = "./Layers/Trimmed_worldclim/Angustifolia/Angustifolia", 
            format = "ascii", bylayer = T, suffix=names(envtStack), NAFlag = "-9999", overwrite = T)

Albescens_env <- mask(envtStack, Albescens)
writeRaster(Albescens_env, filename = "./Layers/Trimmed_worldclim/Albescens/Albescens", 
            format = "ascii", bylayer = T, suffix=names(envtStack), NAFlag = "-9999", overwrite = T)

Barbigera_env <- mask(envtStack, Barbigera)
writeRaster(Barbigera_env, filename = "./Layers/Trimmed_worldclim/Barbigera/Barbigera", 
            format = "ascii", bylayer = T, suffix=names(envtStack), NAFlag = "-9999", overwrite = T)

Bipenicillata_env <- mask(envtStack, Bipenicillata)
writeRaster(Bipenicillata_env, filename = "./Layers/Trimmed_worldclim/Bipenicillata/Bipenicillata", 
            format = "ascii", bylayer = T, suffix=names(envtStack), NAFlag = "-9999", overwrite = T)

Fraterna_env <- mask(envtStack, Fraterna)
writeRaster(Fraterna_env, filename = "./Layers/Trimmed_worldclim/Fraterna/Fraterna", 
            format = "ascii", bylayer = T, suffix=names(envtStack), NAFlag = "-9999", overwrite = T)

Dissitiflora_env <- mask(envtStack, Dissitiflora)
writeRaster(Dissitiflora_env, filename = "./Layers/Trimmed_worldclim/Dissitiflora/Dissitiflora", 
            format = "ascii", bylayer = T, suffix=names(envtStack), NAFlag = "-9999", overwrite = T)

Duidae_env <- mask(envtStack, Duidae)
writeRaster(Duidae_env, filename = "./Layers/Trimmed_worldclim/Duidae/Duidae", 
            format = "ascii", bylayer = T, suffix=names(envtStack), NAFlag = "-9999", overwrite = T)

Melastomoides_env <- mask(envtStack, Melastomoides)
writeRaster(Melastomoides_env, filename = "./Layers/Trimmed_worldclim/Melastomoides/Melastomoides", 
            format = "ascii", bylayer = T, suffix=names(envtStack), NAFlag = "-9999", overwrite = T)

Llanorum_env <- mask(envtStack, Llanorum)
writeRaster(Llanorum_env, filename = "./Layers/Trimmed_worldclim/Llanorum/Llanorum", 
            format = "ascii", bylayer = T, suffix=names(envtStack), NAFlag = "-9999", overwrite = T)

Papyrus_env <- mask(envtStack, Papyrus)
writeRaster(Papyrus_env, filename = "./Layers/Trimmed_worldclim/Papyrus/Papyrus", 
            format = "ascii", bylayer = T, suffix=names(envtStack), NAFlag = "-9999", overwrite = T)

Verticillaris_env <- mask(envtStack, Verticillaris)
writeRaster(Verticillaris_env, filename = "./Layers/Trimmed_worldclim/Verticillaris/Verticillaris", 
            format = "ascii", bylayer = T, suffix=names(envtStack), NAFlag = "-9999", overwrite = T)

Nigricans_env <- mask(envtStack, Nigricans)
writeRaster(Nigricans_env, filename = "./Layers/Trimmed_worldclim/Nigricans/Nigricans", 
            format = "ascii", bylayer = T, suffix=names(envtStack), NAFlag = "-9999", overwrite = T)



######################
#Trim soil layers to extents
#Loading soil layers (specifically ph layer - others already done)

#soilList <- list.files(path = "./Layers/Soil/Resolution/", pattern = ".asc", full.names=TRUE)
#for (i in 5:length(soilList)){
#  soil <- raster(soilList[i])
#  name <- -names(soil)
#  writeRaster(soil, filename = paste0("./Layers/Soil/SoilGrid/", name), format = "ascii", overwrite=TRUE)
#}

soil_ph <- list.files(path = "./Layers/Soil/SoilGrid/", pattern = "HOX", full.names=TRUE)
for (i in 1:length(soil_ph)){
  soil <- raster(soil_ph[i])
  name <- -names(soil)
  writeRaster(soil, filename = paste0("./Layers/Soil/SoilGrid/", name), format = "ascii", overwrite = TRUE)
}

soilList
soil


#Trim soil layers to match training regions
soilRasters <- list.files(path = "./Layers/Soil/SoilGrid/", pattern = ".asc", full.names=TRUE)
soilRasters <- soilRasters[-2]

soilStack <- stack(soilRasters)
crs(soilStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

extent(soilStack) <- extent(-180, 180, -60, 90)
extent(soilStack)

#Masking your environmental variables to M
#Saves all the layers in the stack, with prefix


#Masking the raster stack
Aaegopogon_soil <- mask(soilStack, Aegopogon) 
writeRaster(Aegopogon_soil, filename = "./Layers/Trimmed_soil/Aaegopogon", 
  format = "ascii", bylayer = T, suffix=names(soilStack), NAFlag = "-9999", overwrite = T)
   
Aspera_soil <- mask(soilStack, Aspera) #Masking the raster stack
writeRaster(Aspera_soil, filename = "./Layers/Trimmed_soil/Aspera", 
            format = "ascii", bylayer = T, suffix=names(soilStack), NAFlag = "-9999", overwrite = T)

Barbigera_soil <- mask(soilStack, Barbigera) #Masking the raster stack
writeRaster(Barbigera_soil, filename = "./Layers/Trimmed_soil/Barbigera", 
            format = "ascii", bylayer = T, suffix=names(soilStack), NAFlag = "-9999", overwrite = T)

Melastomoides_soil <- mask(soilStack, Melastomoides) #Masking the raster stack
writeRaster(Melastomoides_soil, filename = "./Layers/Trimmed_soil/Melastomoides", 
            format = "ascii", bylayer = T, suffix=names(soilStack), NAFlag = "-9999", overwrite = T)

Angustifolia_soil <- mask(soilStack, Angustifolia) #Masking the raster stack
writeRaster(Angustifolia_soil, filename = "./Layers/Trimmed_soil/Angustifolia", 
            format = "ascii", bylayer = T, suffix=names(soilStack), NAFlag = "-9999", overwrite = T)

Albescens_soil <- mask(soilStack, Albescens) #Masking the raster stack
writeRaster(Albescens_soil, filename = "./Layers/Trimmed_soil/Albescens", 
            format = "ascii", bylayer = T, suffix=names(soilStack), NAFlag = "-9999", overwrite = T)

Nigricans_soil <- mask(soilStack, Nigricans) #Masking the raster stack
writeRaster(Nigricans_soil, filename = "./Layers/Trimmed_soil/Nigricans", 
            format = "ascii", bylayer = T, suffix=names(soilStack), NAFlag = "-9999", overwrite = T)

Fraterna_soil <- mask(soilStack, Fraterna) #Masking the raster stack
writeRaster(Fraterna_soil, filename = "./Layers/Trimmed_soil/Fraterna", 
            format = "ascii", bylayer = T, suffix=names(soilStack), NAFlag = "-9999", overwrite = T)

Bipenicillata_soil <- mask(soilStack, Bipenicillata) #Masking the raster stack
writeRaster(Bipenicillata_soil, filename = "./Layers/Trimmed_soil/Bipenicillata", 
            format = "ascii", bylayer = T, suffix=names(soilStack), NAFlag = "-9999", overwrite = T)

Dissitiflora_soil <- mask(soilStack, Dissitiflora) #Masking the raster stack
writeRaster(Dissitiflora_soil, filename = "./Layers/Trimmed_soil/Dissitiflora", 
            format = "ascii", bylayer = T, suffix=names(soilStack), NAFlag = "-9999", overwrite = T)

Duidae_soil <- mask(soilStack, Duidae) #Masking the raster stack
writeRaster(Duidae_soil, filename = "./Layers/Trimmed_soil/Duidae", 
            format = "ascii", bylayer = T, suffix=names(soilStack), NAFlag = "-9999", overwrite = T)

Llanorum_soil <- mask(soilStack, Llanorum) #Masking the raster stack
writeRaster(Llanorum_soil, filename = "./Layers/Trimmed_soil/Llanorum", 
            format = "ascii", bylayer = T, suffix=names(soilStack), NAFlag = "-9999", overwrite = T)

Papyrus_soil <- mask(soilStack, Papyrus) #Masking the raster stack
writeRaster(Papyrus_soil, filename = "./Layers/Trimmed_soil/Papyrus", 
            format = "ascii", bylayer = T, suffix=names(soilStack), NAFlag = "-9999", overwrite = T)

Verticillaris_soil <- mask(soilStack, Verticillaris) #Masking the raster stack
writeRaster(Verticillaris_soil, filename = "./Layers/Trimmed_soil/Verticillaris", 
            format = "ascii", bylayer = T, suffix=names(soilStack), NAFlag = "-9999", overwrite = T)

#########

#Get layers for projecting onto
NW <- raster("./Layers/Projecting/Aegopogon/Aegopogon_alt.asc")
plot(NW)
crs(NW)
#Gets a list of .asc files
#envtList <- list.files(path = "./Lab5/WorldClimTerrestrial", pattern = ".asc", full.names = TRUE)
soilList <- list.files(path = "./Layers/Soil/SoilGrid/Reduced", pattern = ".asc", full.names = TRUE)
soilList_red <- soilList[-1]
soilList_red <- soilList_red[-4]
soilList
soilList_red



#Reads in .asc files as a raster stack
soilStack <- stack(soilList_red)
crs(soilStack)
crs(soilStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers
crs(NW) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
#Masking the raster stack
NW_layers <- crop(soilStack, extent(NW))
writeRaster(NW_layers, filename = "./Layers/Projecting/Aegopogon/Aegopogon", 
            format = "ascii", bylayer = T, suffix=names(soilStack), NAFlag = "-9999", overwrite = T)

NW_layers 
extent(NW)
extent(envtStack)
