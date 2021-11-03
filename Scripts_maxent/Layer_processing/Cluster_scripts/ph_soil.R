#Only for ph soil layer

library(raster); #For raster-based loading, calculations, and mapping
library(rgdal); #For reading the M polygon
library(sf)

NW_res <- raster("./Projecting_layers/Aegopogon_bio1.asc")

crs.geo <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
crs(NW_res) <- crs.geo

soilRasters <- list.files(path = "./Raw_soil_layers", pattern = ".asc", full.names=TRUE)

raster7 <- raster(soilRasters[7]) 

crs(raster7) <- crs.geo

raster7_crop <- crop(raster7, extent(NW_res))

writeRaster(raster7_crop, filename = paste0("./Raw_soil_layers/cropped_", names(raster7)), format = "ascii", NAFlag = "-9999", overwrite = T)

#NW_res_crop <- crop(NW_res, extent(raster7_crop))
#raster7_red <- resample(raster7_crop, NW_res)
#writeRaster(raster7_red, filename = paste0("./Raw_soil_layers/Reduced/",names(raster7)),# format = "ascii", NAFlag = "-9999", overwrite = T)


