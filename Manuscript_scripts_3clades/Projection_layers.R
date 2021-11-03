#trim layers for projecting

#read names of layers for each clade

WideList <- list.files(path = "./Layers/3_clades/Wide/Reduced/", pattern = ".asc", full.names = F); #Gets a list of .asc files
SouthList <- list.files(path = "./Layers/3_clades/South/Reduced/", pattern = ".asc", full.names = F); #Gets a list of .asc files
NorthList <- list.files(path = "./Layers/3_clades/North/Reduced", pattern = ".asc", full.names = F); #Gets a list of .asc files

#get rid of underscore in name
WideList <- gsub("_Wide", "Wide", WideList)
SouthList <- gsub("_Wide", "Wide", SouthList)
NorthList <- gsub("_Wide", "Wide", NorthList)

#get short version of name
WideList <- gsub(".asc", "", WideList)
SouthList <- gsub(".asc", "", SouthList)
NorthList <- gsub(".asc", "", NorthList)

#read original layers
soil_layers <- list.files(path = "./Layers/Non_env_layers_raw/To_trim", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
soil_stack <- stack(soil_layers)
crs(soil_stack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers

envt_layers <- list.files(path = "./WorldClimTerrestrial/WorldClimTerrestrial", pattern = ".asc", full.names = TRUE)
envt_stack <- stack(envt_layers)
crs(envt_stack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers

#trim and reduce resolution to "NW" for projecting
NW <- raster("./Layers/Projecting/Aegopogon/Aegopogon_bio10.asc")
crs.geo <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers
crs(NW) <- crs.geo

#for all layers

NW_envt <- crop(envt_stack, extent(NW))

NW_soil <- crop(soil_stack, extent(NW))

combo <- stack(NW_soil, NW_envt)

writeRaster(combo, filename = "./Layers/3_clades/Projecting/North/North", 
            format = "ascii", bylayer = T, suffix=names(combo), NAFlag = "-9999", overwrite = T)
writeRaster(combo, filename = "./Layers/3_clades/Projecting/South/South", 
            format = "ascii", bylayer = T, suffix=names(combo), NAFlag = "-9999", overwrite = T)
writeRaster(combo, filename = "./Layers/3_clades/Projecting/Wide/Wide", 
            format = "ascii", bylayer = T, suffix=names(combo), NAFlag = "-9999", overwrite = T)


