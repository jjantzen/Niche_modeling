library(raster)
#Comparison layer
prism <- raster("croppedbio1.asc")
#Layer you want to change resolution
soil <- raster("soilsand0m.asc")
#A safe measure
trial <- soil
#resampling the soil layer based off of the comparison layer
new <- resample(trial,prism)
#########CHECK THE HEADERS OF THE NEW LAYER
prism

class       : RasterLayer 
dimensions  : 1649, 2036, 3357364  (nrow, ncol, ncell)
resolution  : 0.008333333, 0.008333333  (x, y)
extent      : -92.2125, -75.24583, 24.0625, 37.80417  (xmin, xmax, ymin, ymax)
coord. ref. : NA 
data source : /Users/andrenaranjo/Desktop/PRISM and Soil Data/PresentLayers/TRIMMED PRESENT LAYERS/croppedToTrainingRegion_prism1.asc 
names       : croppedToTrainingRegion_prism1 

new

class       : RasterLayer 
dimensions  : 1649, 2036, 3357364  (nrow, ncol, ncell)
resolution  : 0.008333333, 0.008333333  (x, y)
extent      : -92.2125, -75.24583, 24.0625, 37.80417  (xmin, xmax, ymin, ymax)
coord. ref. : NA 
data source : in memory
names       : croppedToTrainingRegion_Unified_NA_Soil_Map_First_Dominant_Soil_Component_Area_Percentage 
values      : 0, 100.6765  (min, max)

#WRITE RASTER
writeRaster(new, filename = "soilsan0mresample", format = "ascii")

#Done :)