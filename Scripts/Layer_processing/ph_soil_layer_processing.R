#ph soil layer reducing and cropping

library(raster)

NW_res <- raster("./Layers/Projecting/Aegopogon/Aegopogon_bio1.asc")

crs.geo <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

crs(NW_res) <- crs.geo

soil_ph <- raster("./Layers/Soil/SoilGrid/phihox_m_sl1_250m.asc")

soil_ph
NW_res

crs(soil_ph) <- crs.geo

ph_crop <- crop(soil_ph, extent(NW_res))

ph_resamp <- resample(ph_crop, NW_res)

writeRaster(ph_resamp, filename = paste0("./Layers/Soil/SoilGrid/Reduced/",names(soil_ph)), format = "ascii", NAFlag = "-9999", overwrite = T)


## get filesnames (assuming the datasets were downloaded already. <br>
## please see http://thebiobucket.blogspot.co.at/2013/06/use-r-to-bulk-download-digital.html <br>
## on how to download high-resolution DEMs)<br>

files <- dir(pattern = ".asc")
soil_ph <- raster("./Layers/Soil/SoilGrid/phihox_m_sl1_250m.asc")
## function for single file processing mind to replace the PATH to gdalinfo.exe!
# s = division applied to each side of raster, i.e. s = 2 gives 4 tiles, 3 gives 9, etc.
split_raster <- function(file, s = 3) {    
  filename <- gsub(".asc", "", file)
  gdalinfo_str <- paste0("\"C:/OSGeo4W64/bin/gdalinfo.exe\" ", file)    
    # pick size of each side   
  x <- as.numeric(gsub("[^0-9]", "", unlist(strsplit(system(gdalinfo_str, intern = T)[3], ", "))))[1]
  y <- as.numeric(gsub("[^0-9]", "", unlist(strsplit(system(gdalinfo_str, intern = T)[3], ", "))))[2]
  # t is nr. of iterations per side
  t <- s - 1
  for (i in 0:t) {
    for (j in 0:t) {
      # [-srcwin xoff yoff xsize ysize] src_dataset dst_dataset<br>            
      srcwin_str <- paste("-srcwin ", i * x/s, j * y/s, x/s, y/s)
      gdal_str <- paste0("\"C:/OSGeo4W64/bin/gdal_translate.exe\" ", srcwin_str, " ", "\"", file, "\" ", "\"", filename, "_", i, "_", j, ".tif\"")
      system(gdal_str)
      }
    }
  }
## process all files and save to same directory<br>
mapply(split_raster, soil_ph, 2) 

