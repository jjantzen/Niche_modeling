#After creating M shapefiles in QGIS for training region
#Trimming environmental layers to M shapes/training regions

library(raster); #For raster-based loading, calculations, and mapping
library(rgdal); #For reading the M polygon
library(sf)

##Loading an environmental raster stack
#
##Gets a list of .asc files
#envtList <- list.files(path = "./Raw_env_layers/WorldClimTerrestrial", pattern = ".asc", full.names = TRUE)
#
##Reads in .asc files as a raster stack
#envtStack <- stack(envtList)
#
##Defines projection of layers
#crs(envtStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" 

#Get M shapefile

##Reads in shapefile
Aegopogon <- readOGR(dsn = "./Training_regions", layer = "Aegopogon_training_region")
Angustifolia <- readOGR(dsn = "./Training_regions", layer = "Angustifolia_training_region")
Albescens <- readOGR(dsn = "./Training_regions", layer = "Albescens_training_region")
Aspera <- readOGR(dsn = "./Training_regions", layer = "Aspera_training_region")
Barbigera <- readOGR(dsn = "./Training_regions", layer = "Barbigera_training_region")
Fraterna <- readOGR(dsn = "./Training_regions", layer = "Fraterna_training_region")
Melastomoides <- readOGR(dsn = "./Training_regions", layer = "Melastomoides_training_region")
Bipenicillata <- readOGR(dsn = "./Training_regions", layer = "Bipenicillata_training_region")
Papyrus <- readOGR(dsn = "./Training_regions", layer = "Papyrus_training_region")
Dissitiflora <- readOGR(dsn = "./Training_regions", layer = "Dissitiflora_training_region")
Llanorum <- readOGR(dsn = "./Training_regions", layer = "Llanorum_training_region_combined")
Duidae <- readOGR(dsn = "./Training_regions", layer = "Duidae_training_region")
Nigricans <- readOGR(dsn = "./Training_regions", layer = "Nigricans_training_region")
Verticillaris <- readOGR(dsn = "./Training_regions", layer = "Verticillaris_training_region")

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

#Read New World shapefile
NW <- raster("./Projecting_layers/Aegopogon_bio1.asc")
crs(NW) <- crs.geo

#Mask raster to training region
#Aspera_env <- mask(envtStack, Aspera)
#Aspera_env2 <- crop(Aspera_env, extent(NW))
#writeRaster(Aspera_env2, filename = "./Layers/Trimmed_worldclim/Aspera/Aspera", 
#            format = "ascii", bylayer = T, suffix=names(envtStack), NAFlag = "-9999", overwrite = T)

#Aegopogon_env <- mask(envtStack, Aegopogon)
#Aegopogon_env2 <- crop(Aegopogon_env, extent(NW))
#writeRaster(Aegopogon_env2, filename = "./Layers/Trimmed_worldclim/Aegopogon/Aegopogon", 
#            format = "ascii", bylayer = T, suffix=names(envtStack), NAFlag = "-9999", overwrite = T)
#
#Angustifolia_env <- mask(envtStack, Angustifolia)
#Angustifolia_env2 <- crop(Angustifolia_env, extent(NW))
#writeRaster(Angustifolia_env2, filename = "./Layers/Trimmed_worldclim/Angustifolia/Angustifolia", 
#            format = "ascii", bylayer = T, suffix=names(envtStack), NAFlag = "-9999", overwrite = T)
#
#Albescens_env <- mask(envtStack, Albescens)
#Albescens_env2 <- crop(Albescens_env, extent(NW))
#writeRaster(Albescens_env2, filename = "./Layers/Trimmed_worldclim/Albescens/Albescens", 
#            format = "ascii", bylayer = T, suffix=names(envtStack), NAFlag = "-9999", overwrite = T)
#
#Barbigera_env <- mask(envtStack, Barbigera)
#Barbigera_env2 <- crop(Barbigera_env, extent(NW))
#writeRaster(Barbigera_env2, filename = "./Layers/Trimmed_worldclim/Barbigera/Barbigera", 
#            format = "ascii", bylayer = T, suffix=names(envtStack), NAFlag = "-9999", overwrite = T)
#
#Bipenicillata_env <- mask(envtStack, Bipenicillata)
#Bipenicillata_env2 <- crop(Bipenicillata_env, extent(NW))
#writeRaster(Bipenicillata_env2, filename = "./Layers/Trimmed_worldclim/Bipenicillata/Bipenicillata", 
#            format = "ascii", bylayer = T, suffix=names(envtStack), NAFlag = "-9999", overwrite = T)
#
#Fraterna_env <- mask(envtStack, Fraterna)
#Fraterna_env2 <- crop(Fraterna_env, extent(NW))
#writeRaster(Fraterna_env2, filename = "./Layers/Trimmed_worldclim/Fraterna/Fraterna", 
#            format = "ascii", bylayer = T, suffix=names(envtStack), NAFlag = "-9999", overwrite = T)
#
#Dissitiflora_env <- mask(envtStack, Dissitiflora)
#Dissitiflora_env2 <- crop(Dissitiflora_env, extent(NW))
#writeRaster(Dissitiflora_env2, filename = "./Layers/Trimmed_worldclim/Dissitiflora/Dissitiflora", 
#            format = "ascii", bylayer = T, suffix=names(envtStack), NAFlag = "-9999", overwrite = T)
#
#Duidae_env <- mask(envtStack, Duidae)
#Duidae_env2 <- crop(Duidae_env, extent(NW))
#writeRaster(Duidae_env2, filename = "./Layers/Trimmed_worldclim/Duidae/Duidae", 
#            format = "ascii", bylayer = T, suffix=names(envtStack), NAFlag = "-9999", overwrite = T)
#
#Melastomoides_env <- mask(envtStack, Melastomoides)
#Melastomoides_env2 <- crop(Melastomoides_env, extent(NW))
#writeRaster(Melastomoides_env2, filename = "./Layers/Trimmed_worldclim/Melastomoides/Melastomoides", 
#            format = "ascii", bylayer = T, suffix=names(envtStack), NAFlag = "-9999", overwrite = T)
#
#Llanorum_env <- mask(envtStack, Llanorum)
#Llanorum_env2 <- crop(Llanorum_env, extent(NW))
#writeRaster(Llanorum_env2, filename = "./Layers/Trimmed_worldclim/Llanorum/Llanorum", 
#            format = "ascii", bylayer = T, suffix=names(envtStack), NAFlag = "-9999", overwrite = T)
#
#Papyrus_env <- mask(envtStack, Papyrus)
#Papyrus_env2 <- crop(Papyrus_env, extent(NW))
#writeRaster(Papyrus_env2, filename = "./Layers/Trimmed_worldclim/Papyrus/Papyrus", 
#            format = "ascii", bylayer = T, suffix=names(envtStack), NAFlag = "-9999", overwrite = T)
#
#Verticillaris_env <- mask(envtStack, Verticillaris)
#Verticillaris_env2 <- crop(Verticillaris_env, extent(NW))
#writeRaster(Verticillaris_env2, filename = "./Layers/Trimmed_worldclim/Verticillaris/Verticillaris", 
#            format = "ascii", bylayer = T, suffix=names(envtStack), NAFlag = "-9999", overwrite = T)
#
#Nigricans_env <- mask(envtStack, Nigricans)
#Nigricans_env2 <- crop(Nigricans_env, extent(NW))
#writeRaster(Nigricans_env2, filename = "./Layers/Trimmed_worldclim/Nigricans/Nigricans", 
#            format = "ascii", bylayer = T, suffix=names(envtStack), NAFlag = "-9999", overwrite = T)
#

######################
#Trim soil layers to extents and mask to training regions

#Masking your environmental variables to M
#Cropping to extent of NW
#Saves all the layers in the stack, with prefix

#Read new resolution soilstack
soilRasters <- list.files(path = "./Raw_soil_layers/Reduced/", pattern = ".asc", full.names=TRUE)
soilStack <- stack(soilRasters)

#Use new resolution soilstack for masking/cropping

Aegopogon_soil <- mask(soilStack, Aegopogon)
Aegopogon_soil2 <- crop(Aegopogon_soil, extent(NW)) 
writeRaster(Aegopogon_soil2, filename = "./Trimmed_soil_layers/Aegopogon/", format = "ascii", bylayer = T, suffix=names(soilStack), NAFlag = "-9999", overwrite = T)
   
Aspera_soil <- mask(soilStack, Aspera) #Masking the raster stack
Aspera_soil2 <- crop(Aspera_soil, extent(NW)) 
writeRaster(Aspera_soil2, filename = "./Trimmed_soil_layers/Aspera/", format = "ascii", bylayer = T, suffix=names(soilStack), NAFlag = "-9999", overwrite = T)

Barbigera_soil <- mask(soilStack, Barbigera) #Masking the raster stack
Barbigera_soil2 <- crop(Barbigera_soil, extent(NW)) 
writeRaster(Barbigera_soil2, filename = "./Trimmed_soil_layers/Barbigera/", format = "ascii", bylayer = T, suffix=names(soilStack), NAFlag = "-9999", overwrite = T)

Melastomoides_soil <- mask(soilStack, Melastomoides) #Masking the raster stack
Melastomoides_soil2 <- crop(Melastomoides_soil, extent(NW)) 
writeRaster(Melastomoides_soil2, filename = "./Trimmed_soil_layers/Melastomoides/", format = "ascii", bylayer = T, suffix=names(soilStack), NAFlag = "-9999", overwrite = T)

Angustifolia_soil <- mask(soilStack, Angustifolia) #Masking the raster stack
Angustifolia_soil2 <- crop(Angustifolia_soil, extent(NW)) 
writeRaster(Angustifolia_soil2, filename = "./Trimmed_soil_layers/Angustifolia/", format = "ascii", bylayer = T, suffix=names(soilStack), NAFlag = "-9999", overwrite = T)

Albescens_soil <- mask(soilStack, Albescens) #Masking the raster stack
Albescens_soil2 <- crop(Albescens_soil, extent(NW)) 
writeRaster(Albescens_soil2, filename = "./Trimmed_soil_layers/Albescens/", format = "ascii", bylayer = T, suffix=names(soilStack), NAFlag = "-9999", overwrite = T)

Nigricans_soil <- mask(soilStack, Nigricans) #Masking the raster stack
Nigricans_soil2 <- crop(Nigricans_soil, extent(NW)) 
writeRaster(Nigricans_soil2, filename = "./Trimmed_soil_layers/Nigricans/", format = "ascii", bylayer = T, suffix=names(soilStack), NAFlag = "-9999", overwrite = T)

Fraterna_soil <- mask(soilStack, Fraterna) #Masking the raster stack
Fraterna_soil2 <- crop(Fraterna_soil, extent(NW)) 
writeRaster(Fraterna_soil2, filename = "./Trimmed_soil_layers/Fraterna/", format = "ascii", bylayer = T, suffix=names(soilStack), NAFlag = "-9999", overwrite = T)

Bipenicillata_soil <- mask(soilStack, Bipenicillata) #Masking the raster stack
Bipenicillata_soil2 <- crop(Bipenicillata_soil, extent(NW)) 
writeRaster(Bipenicillata_soil2, filename = "./Trimmed_soil_layers/Bipenicillata/", format = "ascii", bylayer = T, suffix=names(soilStack), NAFlag = "-9999", overwrite = T)

Dissitiflora_soil <- mask(soilStack, Dissitiflora) #Masking the raster stack
Dissitiflora_soil2 <- crop(Dissitiflora_soil, extent(NW)) 
writeRaster(Dissitiflora_soil2, filename = "./Trimmed_soil_layers/Dissitiflora/", format = "ascii", bylayer = T, suffix=names(soilStack), NAFlag = "-9999", overwrite = T)

Duidae_soil <- mask(soilStack, Duidae) #Masking the raster stack
Duidae_soil2 <- crop(Duidae_soil, extent(NW)) 
writeRaster(Duidae_soil2, filename = "./Trimmed_soil_layers/Duidae/", format = "ascii", bylayer = T, suffix=names(soilStack), NAFlag = "-9999", overwrite = T)

Llanorum_soil <- mask(soilStack, Llanorum) #Masking the raster stack
Llanorum_soil2 <- crop(Llanorum_soil, extent(NW)) 
writeRaster(Llanorum_soil2, filename = "./Trimmed_soil_layers/Llanorum/", format = "ascii", bylayer = T, suffix=names(soilStack), NAFlag = "-9999", overwrite = T)

Papyrus_soil <- mask(soilStack, Papyrus) #Masking the raster stack
Papyrus_soil2 <- crop(Papyrus_soil, extent(NW)) 
writeRaster(Papyrus_soil2, filename = "./Trimmed_soil_layers/Papyrus/", format = "ascii", bylayer = T, suffix=names(soilStack), NAFlag = "-9999", overwrite = T)

Verticillaris_soil <- mask(soilStack, Verticillaris) #Masking the raster stack
Verticillaris_soil2 <- crop(Verticillaris_soil, extent(NW)) 
writeRaster(Verticillaris_soil2, filename = "./Trimmed_soil_layers/Verticillaris/", format = "ascii", bylayer = T, suffix=names(soilStack), NAFlag = "-9999", overwrite = T)
