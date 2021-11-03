#After creating M shapefiles in QGIS for training region
#Trimming environmental layers to M shapes/training regions

library(raster); #For raster-based loading, calculations, and mapping
library(rgdal); #For reading the M polygon
library(sf)

#Loading an environmental raster stack

#Gets a list of .asc files
envtList <- list.files(path = "./WorldClimTerrestrial/WorldClimTerrestrial", pattern = ".asc", full.names = TRUE)

#Reads in .asc files as a raster stack
envtStack <- stack(envtList)

#Defines projection of layers
crs(envtStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" 

#Get M shapefile

#Reads in shapefile
north_range <- readOGR(dsn = "./Data/3_clades", layer = "north_range_splink_ident")
south_range <- readOGR(dsn = "./Data/3_clades", layer = "south_range_splink_ident")
wide_range <- readOGR(dsn = "./Data/3_clades", layer = "wide_range_splink_ident")

#Defines M projection as identical to envtStack
crs.geo <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

crs(north_range) <- crs.geo
crs(south_range) <- crs.geo
crs(wide_range) <- crs.geo

#Read New World shapefile
NW <- raster("./Layers/Projecting/Aegopogon/Aegopogon_bio10.asc")
crs(NW) <- crs.geo

#Mask raster to training region
north_env <- mask(envtStack, north_range)
north_env2 <- crop(north_env, extent(NW))
writeRaster(north_env2, filename = "./Layers/3_clades/North_splink_ident/North_splink_ident", 
            format = "ascii", bylayer = T, suffix=names(envtStack), NAFlag = "-9999", overwrite = T)

south_env <- mask(envtStack, south_range)
south_env2 <- crop(south_env, extent(NW))
writeRaster(south_env2, filename = "./Layers/3_clades/South_splink_ident/South_splink_ident", 
            format = "ascii", bylayer = T, suffix=names(envtStack), NAFlag = "-9999", overwrite = T)


wide_env <- mask(envtStack, wide_range)
wide_env2 <- crop(wide_env, extent(NW))
writeRaster(wide_env2, filename = "./Layers/3_clades/Wide_splink_ident/Wide_splink_ident", 
            format = "ascii", bylayer = T, suffix=names(envtStack), NAFlag = "-9999", overwrite = T)


######################
#Trim soil layers to extents and mask to training regions

NW_res <- raster("./Layers/3_clades/North_splink_ident/North_splink_ident_alt.asc")
crs.geo <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
crs(NW_res) <- crs.geo

#Loading layers to trim
otherRasters <- list.files(path = "./Layers/Non_env_layers_raw/To_trim/", pattern = ".asc", full.names=TRUE)
old_otherRasters_stack <- stack(otherRasters)

#Trim soil layers to match training regions
crs(old_otherRasters_stack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

#extent(old_otherRasters_stack) <- extent(-180, 180, -60, 90)

#Match resolution to worldclim layers
#Masking your environmental variables to M
#Cropping to extent of NW
#Saves all the layers in the stack, with prefix

#Resample soilstack

#Read envt worldclim layer for resolution reference
envt_res <- raster("./Layers/3_clades/North_splink_ident/North_splink_ident_alt.asc")

res(envt_res)
res(old_otherRasters_stack)

#Use new resolution layer stack for masking/cropping
north_other <- mask(old_otherRasters_stack, north_range)
north_other2 <- crop(north_other, extent(NW_res)) 
writeRaster(north_other2, filename = "./Layers/3_clades/North_splink_ident/North_splink_ident", 
            format = "ascii", bylayer = T, suffix=names(old_otherRasters_stack), NAFlag = "-9999", overwrite = T)

south_other <- mask(old_otherRasters_stack, south_range) #Masking the raster stack
south_other2 <- crop(south_other, extent(NW_res)) 
writeRaster(south_other2, filename = "./Layers/3_clades/South_splink_ident/South_splink_ident", 
            format = "ascii", bylayer = T, suffix=names(old_otherRasters_stack), NAFlag = "-9999", overwrite = T)

wide_other <- mask(old_otherRasters_stack, wide_range) #Masking the raster stack
wide_other2 <- crop(wide_other, extent(NW_res)) 
writeRaster(wide_other2, filename = "./Layers/3_clades/Wide_splink_ident/Wide_splink_ident", 
            format = "ascii", bylayer = T, suffix=names(old_otherRasters_stack), NAFlag = "-9999", overwrite = T)
