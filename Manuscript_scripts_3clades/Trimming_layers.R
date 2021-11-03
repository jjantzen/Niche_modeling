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
north_range <- readOGR(dsn = "./Data/3_clades", layer = "north_range")
south_range <- readOGR(dsn = "./Data/3_clades", layer = "south_range")
wide_range <- readOGR(dsn = "./Data/3_clades", layer = "wide_range")

#Defines M projection as identical to envtStack
crs.geo <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

crs(north_range) <- crs.geo
crs(south_range) <- crs.geo
crs(wide_range) <- crs.geo

#Read New World shapefile
#NW <- raster("./Layers/Projecting/Aegopogon/Aegopogon_bio10.asc")
#crs(NW) <- crs.geo

#Mask raster to training region
north_env <- mask(envtStack, north_range)
north_env2 <- crop(north_env, extent(NW))
writeRaster(north_env2, filename = "./Layers/3_clades/North", 
            format = "ascii", bylayer = T, suffix=names(envtStack), NAFlag = "-9999", overwrite = T)

south_env <- mask(envtStack, south_range)
south_env2 <- crop(south_env, extent(NW))
writeRaster(south_env2, filename = "./Layers/3_clades/South", 
            format = "ascii", bylayer = T, suffix=names(envtStack), NAFlag = "-9999", overwrite = T)


wide_env <- mask(envtStack, wide_range)
wide_env2 <- crop(wide_env, extent(NW))
writeRaster(wide_env2, filename = "./Layers/3_clades/Wide", 
            format = "ascii", bylayer = T, suffix=names(envtStack), NAFlag = "-9999", overwrite = T)


######################
#Trim soil layers to extents and mask to training regions

NW_res <- raster("./Layers/3_clades/North/North_alt.asc")
crs.geo <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
crs(NW_res) <- crs.geo

#Convert layers from tif to asc
ai_tif <- raster("./Layers/Non_env_layers_raw/global-ai_et0/ai_et0/ai_et0.tif")
ai_crop <- crop(ai_tif, extent(NW_res))
ai_resamp <- resample(ai_crop, NW_res)
writeRaster(ai_resamp, filename = "./Layers/Non_env_layers_raw/To_trim/ai_et0.asc", format = "ascii", overwrite = TRUE)

et_tif <- raster("./Layers/Non_env_layers_raw/global-et0_annual.tif/et0_yr/et0_yr.tif")
et_crop <- crop(et_tif, extent(NW_res))
et_resamp <- resample(et_crop, NW_res)
writeRaster(et_resamp, filename = "./Layers/Non_env_layers_raw/To_trim/et0_yr.asc", format = "ascii", overwrite = TRUE)

#crop to same extents as canopy
bdticm <- raster("./Layers/Non_env_layers_raw/To_trim/bdticm_resolution2.asc")
bldfie <- raster("./Layers/Non_env_layers_raw/To_trim/bldfie_resolution2.asc")
sndppt <- raster("./Layers/Non_env_layers_raw/To_trim/sndppt_resolution2.asc")
canopy <- raster("./Layers/Non_env_layers_raw/To_trim/Originals/Canopy_NW_extent.asc")

plot(canopy)



bdticm_crop <- crop(bdticm, extent(NW_res))
bldfie_crop <- crop(bldfie, extent(NW_res))
sndppt_crop <- crop(sndppt, extent(NW_res))
canopy_crop <- crop(canopy, extent(NW_res))

writeRaster(bdticm_crop, filename = "./Layers/Non_env_layers_raw/To_trim/bdticm_trimmed.asc", format = "ascii", overwrite = TRUE)
writeRaster(bldfie_crop, filename = "./Layers/Non_env_layers_raw/To_trim/bldfie_trimmed.asc", format = "ascii", overwrite = TRUE)
writeRaster(sndppt_crop, filename = "./Layers/Non_env_layers_raw/To_trim/sndppt_trimmed.asc", format = "ascii", overwrite = TRUE)
writeRaster(canopy_crop, filename = "./Layers/Non_env_layers_raw/To_trim/canopy_trimmed.asc", format = "ascii", overwrite = TRUE)


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
envt_res <- raster("./Layers/3_clades/North/North_alt.asc")

res(envt_res)
res(old_otherRasters_stack)

#Use new resolution layer stack for masking/cropping


north_other <- mask(old_otherRasters_stack, north_range)
north_other2 <- crop(north_other, extent(NW_res)) 
writeRaster(north_other2, filename = "./Layers/3_clades/North", 
            format = "ascii", bylayer = T, suffix=names(old_otherRasters_stack), NAFlag = "-9999", overwrite = T)

south_other <- mask(old_otherRasters_stack, south_range) #Masking the raster stack
south_other2 <- crop(south_other, extent(NW_res)) 
writeRaster(south_other2, filename = "./Layers/3_clades/South", 
            format = "ascii", bylayer = T, suffix=names(old_otherRasters_stack), NAFlag = "-9999", overwrite = T)

wide_other <- mask(old_otherRasters_stack, wide_range) #Masking the raster stack
wide_other2 <- crop(wide_other, extent(NW_res)) 
writeRaster(wide_other2, filename = "./Layers/3_clades/Wide/Wide", 
            format = "ascii", bylayer = T, suffix=names(old_otherRasters_stack), NAFlag = "-9999", overwrite = T)
