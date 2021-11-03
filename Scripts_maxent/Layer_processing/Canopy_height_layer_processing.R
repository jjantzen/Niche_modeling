#Processing canopy height layer and other vegetation layers

library(raster); #For raster-based loading, calculations, and mapping
library(rgdal); #For reading the M polygon
library(sf)
library(dplyr)
library(rgdal)
library(raster)
library(scrubr)
library(spatstat)

#Read layer raster file
canopy <- raster("./Layers/Vegetation/canopy_3.asc")

# veg_list1 <- list.files(path = "./Layers/Vegetation", pattern = ".asc", full.names = TRUE)
# veg_list2 <- list.files(path = "./Layers/Vegetation", pattern = ".tif", full.names = TRUE)
# veg_list2
# veg_list2 <- veg_list2[-12]
# veg_list2 <- veg_list2[-4]
# veg_list2 <- veg_list2[-2]
# veg_list2
# veg_list <- c(veg_list1, veg_list2)
# 
# vegetation_stack <- stack(veg_list)

#Get M shapefile

##Reads in shapefile
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
#Nigricans <- readOGR(dsn = "./Layers/Training_regions", layer = "Nigricans_training_region")
Verticillaris <- readOGR(dsn = "./Layers/Training_regions", layer = "Verticillaris_training_region")

#Defines M projection as identical to envtStack
crs.geo <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
crs(canopy) <- crs.geo

crs(Aegopogon) <- crs.geo
crs(Aspera) <- crs.geo
crs(Angustifolia) <- crs.geo
crs(Albescens) <- crs.geo
crs(Fraterna) <- crs.geo
crs(Barbigera) <- crs.geo
crs(Melastomoides) <- crs.geo
crs(Bipenicillata) <- crs.geo
crs(Llanorum) <- crs.geo
#crs(Nigricans) <- crs.geo
crs(Papyrus) <- crs.geo
crs(Dissitiflora) <- crs.geo
crs(Duidae) <- crs.geo
crs(Verticillaris) <- crs.geo

#Read New World shapefile
NW <- raster("./Layers/Projecting/Aegopogon/Aegopogon_alt.asc")
crs(NW) <- crs.geo
#compareRaster(NW, canopy)

extent(NW)
extent(canopy)
#Mask raster to training region
Aspera_can <- mask(canopy, Aspera)
Aspera_can2 <- crop(Aspera_can, extent(NW))
writeRaster(Aspera_can2, filename = "./Layers/Vegetation/Trimmed/Aspera_canopy",
           format = "ascii", NAFlag = "-9999", overwrite = T)

Aegopogon_can <- mask(canopy, Aegopogon)
Aegopogon_can2 <- crop(Aegopogon_can, extent(NW))
writeRaster(Aegopogon_can2, filename = "./Layers/Vegetation/Trimmed/Aegopogon_canopy",
           format = "ascii", NAFlag = "-9999", overwrite = T)

Angustifolia_can <- mask(canopy, Angustifolia)
Angustifolia_can2 <- crop(Angustifolia_can, extent(NW))
writeRaster(Angustifolia_can2, filename = "./Layers/Vegetation/Trimmed/Angustifolia_canopy",
           format = "ascii", NAFlag = "-9999", overwrite = T)

Albescens_can <- mask(canopy, Albescens)
Albescens_can2 <- crop(Albescens_can, extent(NW))
writeRaster(Albescens_can2, filename = "./Layers/Vegetation/Trimmed/Albescens_canopy",
           format = "ascii", NAFlag = "-9999", overwrite = T)

Barbigera_can <- mask(canopy, Barbigera)
Barbigera_can2 <- crop(Barbigera_can, extent(NW))
writeRaster(Barbigera_can2, filename = "./Layers/Vegetation/Trimmed/Barbigera_canopy",
           format = "ascii", NAFlag = "-9999", overwrite = T)

Bipenicillata_can <- mask(canopy, Bipenicillata)
Bipenicillata_can2 <- crop(Bipenicillata_can, extent(NW))
writeRaster(Bipenicillata_can2, filename = "./Layers/Vegetation/Trimmed/Bipenicillata_canopy",
           format = "ascii", NAFlag = "-9999", overwrite = T)

Fraterna_can <- mask(canopy, Fraterna)
Fraterna_can2 <- crop(Fraterna_can, extent(NW))
writeRaster(Fraterna_can2, filename = "./Layers/Vegetation/Trimmed/Fraterna_canopy",
           format = "ascii", NAFlag = "-9999", overwrite = T)

Dissitiflora_can <- mask(canopy, Dissitiflora)
Dissitiflora_can2 <- crop(Dissitiflora_can, extent(NW))
writeRaster(Dissitiflora_can2, filename = "./Layers/Vegetation/Trimmed/Dissitiflora_canopy",
           format = "ascii", NAFlag = "-9999", overwrite = T)

Duidae_can <- mask(canopy, Duidae)
Duidae_can2 <- crop(Duidae_can, extent(NW))
writeRaster(Duidae_can2, filename = "./Layers/Vegetation/Trimmed/Duidae_canopy",
           format = "ascii", NAFlag = "-9999", overwrite = T)

Melastomoides_can <- mask(canopy, Melastomoides)
Melastomoides_can2 <- crop(Melastomoides_can, extent(NW))
writeRaster(Melastomoides_can2, filename = "./Layers/Vegetation/Trimmed/Melastomoides_canopy",
           format = "ascii", NAFlag = "-9999", overwrite = T)

Llanorum_can <- mask(canopy, Llanorum)
Llanorum_can2 <- crop(Llanorum_can, extent(NW))
writeRaster(Llanorum_can2, filename = "./Layers/Vegetation/Trimmed/Llanorum_canopy",
           format = "ascii", NAFlag = "-9999", overwrite = T)

Papyrus_can <- mask(canopy, Papyrus)
Papyrus_can2 <- crop(Papyrus_can, extent(NW))
writeRaster(Papyrus_can2, filename = "./Layers/Vegetation/Trimmed/Papyrus_canopy",
           format = "ascii", NAFlag = "-9999", overwrite = T)

Verticillaris_can <- mask(canopy, Verticillaris)
Verticillaris_can2 <- crop(Verticillaris_can, extent(NW))
writeRaster(Verticillaris_can2, filename = "./Layers/Vegetation/Trimmed/Verticillaris_canopy",
           format = "ascii", NAFlag = "-9999", overwrite = T)

# Nigricans_env <- mask(envtStack, Nigricans)
# Nigricans_env2 <- crop(Nigricans_env, extent(NW))
# writeRaster(Nigricans_env2, filename = "./Layers/Trimmed_worldclim/Nigricans/Nigricans",
#            format = "ascii", bylayer = T, suffix=names(envtStack), NAFlag = "-9999", overwrite = T)

#Trim to extent of NW and save
canopy_crop <- crop(canopy, extent(NW))

writeRaster(canopy_crop, filename = "./Layers/Vegetation/Trimmed/Canopy_NW_extent", format = "ascii", NAFlag = "-9999", overwrite = T)
