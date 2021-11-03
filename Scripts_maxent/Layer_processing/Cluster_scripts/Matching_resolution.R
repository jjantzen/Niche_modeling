library(raster); #For raster-based loading, calculations, and mapping
library(rgdal); #For reading the M polygon
library(sf)

crs.geo <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

#Convert pH layer from tif to asc
#soil_ph <- raster("./Raw_soil_layers/PHIHOX_M_sl1_250m.tif")
#
#writeRaster(soil_ph, filename = "./Raw_soil_layers/phihox_m_sl1_250m.asc", format = "ascii", overwrite = TRUE)

#Read envt worldclim layer for resolution reference
NW_res <- raster("./Projecting_layers/Aegopogon_bio1.asc")

crs(NW_res) <- crs.geo

#extent(envt_res) <- extent(-180, 180, -60, 90)

#Loading soil layers
soilRasters <- list.files(path = "./Raw_soil_layers", pattern = ".asc", full.names=TRUE)
raster1 <- raster(soilRasters[1])
raster2 <- raster(soilRasters[2])
raster3 <- raster(soilRasters[3])
raster4 <- raster(soilRasters[4])
raster5 <- raster(soilRasters[5])
raster6 <- raster(soilRasters[6])
raster7 <- raster(soilRasters[7]) 

crs(raster1) <- crs.geo
crs(raster2) <- crs.geo
crs(raster3) <- crs.geo
crs(raster4) <- crs.geo
crs(raster5) <- crs.geo
crs(raster6) <- crs.geo
crs(raster7) <- crs.geo

#raster1_crop <- crop(raster1, extent(NW_res))
#NW_res_crop <- crop(NW_res, extent(raster1_crop))
#raster1_red <- resample(raster1_crop, NW_res_crop)
#writeRaster(raster1_red, filename = paste0("./Raw_soil_layers/Reduced/",names(raster1)),# format = "ascii", NAFlag = "-9999", overwrite = T)

raster2_crop <- crop(raster2, extent(NW_res))
NW_res_crop <- crop(NW_res, extent(raster2_crop))
raster2_red <- resample(raster2_crop, NW_res_crop)
writeRaster(raster2_red, filename = paste0("./Raw_soil_layers/Reduced/",names(raster2)), format = "ascii", NAFlag = "-9999", overwrite = T)

raster3_crop <- crop(raster3, extent(NW_res))
NW_res_crop <- crop(NW_res, extent(raster3_crop))
raster3_red <- resample(raster3_crop, NW_res_crop)
writeRaster(raster3_red, filename = paste0("./Raw_soil_layers/Reduced/",names(raster3)), format = "ascii", NAFlag = "-9999", overwrite = T)

raster4_crop <- crop(raster4, extent(NW_res))
NW_res_crop <- crop(NW_res, extent(raster4_crop))
raster4_red <- resample(raster4_crop, NW_res_crop)
writeRaster(raster4_red, filename = paste0("./Raw_soil_layers/Reduced/",names(raster4)), format = "ascii", NAFlag = "-9999", overwrite = T)

raster5_crop <- crop(raster5, extent(NW_res))
NW_res_crop <- crop(NW_res, extent(raster5_crop))
raster5_red <- resample(raster5_crop, NW_res_crop)
writeRaster(raster5_red, filename = paste0("./Raw_soil_layers/Reduced/",names(raster5)), format = "ascii", NAFlag = "-9999", overwrite = T)

raster6_crop <- crop(raster6, extent(NW_res))
NW_res_crop <- crop(NW_res, extent(raster6_crop))
raster6_red <- resample(raster6_crop, NW_res_crop)
writeRaster(raster6_red, filename = paste0("./Raw_soil_layers/Reduced/",names(raster6)), format = "ascii", NAFlag = "-9999", overwrite = T)

raster7_crop <- crop(raster7, extent(NW_res))
#NW_res_crop <- crop(NW_res, extent(raster7_crop))
raster7_red <- resample(raster7_crop, NW_res_crop)
writeRaster(raster7_red, filename = paste0("./Raw_soil_layers/Reduced/",names(raster7)), format = "ascii", NAFlag = "-9999", overwrite = T)

#raster 7 has different resolution so doesn't stack with the others
#raster_list <- list(raster1, raster2, raster3, raster4, raster5, raster6, raster7)

#for (i in 1:length(soilRasters)){
#  soil_raster <- raster(soilRasters[i])
#  crs(soil_raster) <- crs.geo
 # extent(soil_raster) <- extent(-180, 180, -60, 90) 
#  soil_raster_red <- resample(soil_raster, envt_res)
#  writeRaster(soil_raster_red, filename = "./Raw_soil_layers/Reduced/", format = "ascii", bylayer #= T, suffix=names(soilRasters[i]), NAFlag = "-9999", overwrite = T)
#}






#old_soilStack <- stack(raster_list)

#crs(old_soilStack) <- crs.geo
#crs(raster7) <- crs.geo

#extent(old_soilStack) <- extent(-180, 180, -60, 90)

#Resample soilstack



#soilStack <- resample(old_soilStack, envt_res)
#raster7_res <- resample(raster7, envt_res)

#Write soilstack with new resolution
#writeRaster(soilStack, filename = "./Raw_soil_layers/Reduced", format = "ascii", bylayer = T,# #suffix=names(soilStack), NAFlag = "-9999", overwrite = T)

#writeRaster(raster7, filename = "./Raw_soil_layers/Reduced", format = "ascii", bylayer = T,## #suffix=names(raster7), NAFlag = "-9999", overwrite = T)
            
