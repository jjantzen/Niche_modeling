

#Only for ph soil layer

library(raster); #For raster-based loading, calculations, and mapping
library(rgdal); #For reading the M polygon
library(sf)

NW_res <- raster("./Layers/3_clades/North_alt.asc")

crs.geo <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
crs(NW_res) <- crs.geo

ph_soil <- raster("./Layers/Soil/SoilGrid/phihox_m_sl1_250m.asc")

crs(ph_soil) <- crs.geo

ph_soil_crop <- crop(ph_soil, extent(NW_res))

writeRaster(raster7_crop, filename = paste0("./Raw_soil_layers/cropped_", names(raster7)), format = "ascii", NAFlag = "-9999", overwrite = T)

#NW_res_crop <- crop(NW_res, extent(raster7_crop))
#raster7_red <- resample(raster7_crop, NW_res)
#writeRaster(raster7_red, filename = paste0("./Raw_soil_layers/Reduced/",names(raster7)),# format = "ascii", NAFlag = "-9999", overwrite = T)


