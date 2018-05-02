#Reducing variables, removing correlated variables
#Removing correlated variables
library(raster)
#Get all environmental and soil layers loaded


TaegSoilList <- list.files(path = "./Layers/Taegopogon/Trimmed_Taegopogon", pattern = "2.asc", full.names = TRUE); #Gets a list of .asc files
TaegSoilStack <- stack(TaegSoilList)
extent(TaegSoilStack) <- extent(-180, 180, -60, 90)
extent(TaegSoilStack)
res(TaegSoilStack)
crs(TaegSoilStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers
TaegSoilList
TaegEnvList <- list.files(path = "./Layers/Taegopogon/Trimmed_Taegopogon", pattern = ".asc", full.names = TRUE)
TaegEnvStack <- stack(TaegEnvList)
extent(TaegEnvStack)
crs(TaegEnvStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers

#Extract info for points
points_Taeg <- read.csv("./Data/clean_Taeg_PointsSPOCC.csv", stringsAsFactors = FALSE)
Soil_raster <- raster("./Layers/Taegopogon/Trimmed_Taegopogon/Taegopogon_sndppt_resolution2.asc")
soil_data_for_points <- extract(Soil_raster, points_Taeg[,2:3])
soil_data_for_points
nrow(points_Taeg)

points_Tasp <- read.csv("./Data/clean_Tasp_PointsSPOCC_no_outlier.csv", stringsAsFactors = FALSE)
Soil_raster <- raster("./Layers/Taspera/Small_Taspera/Taspera_sndppt_resolution2.asc")
soil_data_for_points <- extract(Soil_raster, points_Tasp[,2:3])
soil_data_for_points




nrow(TaegSoilStack)
ncol(TaegSoilStack$Taegopogon_sndppt_resolution)
nrow(TaegEnvStack)
ncol(TaegEnvStack)
extent(TaegEnvStack)
res(TaegEnvStack)
TaegEnvStack <- extend(TaegEnvStack, TaegSoilStack)
ncol(TaegSoilStack)
plot(TaegSoilStack)
TaegStack <- addLayer(TaegEnvStack, TaegSoilStack$Taegopogon_bdticm_resolution, TaegSoilStack$Taegopogon_bldfie_resolution, TaegSoilStack$Taegopogon_sndppt_resolution)
compareRaster(TaegEnvStack, TaegSoilStack)

T_soil <- raster("./Layers/Taegopogon/Trimmed_Taegopogon/Taegopogon_bdticm_resolution2.asc")
T_alt <- raster("./Layers/Taegopogon/Trimmed_Taegopogon/Taegopogon_alt.asc")
compareRaster(T_soil, T_alt)
extent(T_soil)
ncol(T_soil)
T_soil <- as.raster(T_soil[,-c(8641:8642)])
plot(T_soil)
plot(T_env)
T_env <- raster("./Layers/Taegopogon/Trimmed_Taegopogon/Taegopogon_alt.asc")
extent(T_env)

TaegStack <- stack(TaegList); #Reads in .asc files as a raster stack
extent(TaegStack) <- extent(-180, 180, -60, 90)

crs(TaegStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers

pairs(TaegSoilStack)


pairs(TaegSoilStack)
#Shows correlations among variables. Inspect visually. Which layers are highly correlated? 
reduced_TaegStack <- TaegStack[[-16:-17]]
#Removes Bio 10 through Bio 13.
pairs(reduced_TaegStack)
summary(reduced_TaegStack)
#Removes Bio 17 through Bio 19
pairs(reduced_TaegStack)
reduced_TaegStack <- reduced_TaegStack[[-6:-8]]
reduced_TaegStack <- reduced_TaegStack[[-4]]
#Removes Bio 5 through Bio 7
pairs(reduced_TaegStack)
reduced_TaegStack <- reduced_TaegStack[[-13:-14]]
pairs(reduced_TaegStack)
reduced_TaegStack <- reduced_TaegStack[[-11]]
pairs(reduced_TaegStack)
#Ok, now none of the variables have a correlation coefficient of 0.90.
writeRaster(reduced_TaegStack, filename = "./Layers/Taegopogon/", 
            format = "ascii", bylayer = T, suffix=names(reduced_TaegStack), NAFlag = "-9999", overwrite = T)
#Saved reduced set if you don't want to hunt for the variables you want to get rid of.

#Get stack for Taspera
TaspList <- list.files(path = "./Layers/Taspera/Small_Taspera/", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
TaspStack <- stack(TaspList); #Reads in .asc files as a raster stack
crs(TaspStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers
plot(TaspStack)
#Check for correlations and remove correlated variables
pairs(TaspStack)
reduced_TaspStack <- TaspStack[[-19:-20]]
reduced_TaspStack <- reduced_TaspStack[[-17]]
reduced_TaspStack <- reduced_TaspStack[[-14]]
reduced_TaspStack <- reduced_TaspStack[[-5:-7]]
reduced_TaspStack <- reduced_TaspStack[[-2:-3]]
pairs(reduced_TaspStack)
writeRaster(reduced_TaspStack, filename = "./Layers/Taspera/", 
            format = "ascii", bylayer = T, suffix=names(reduced_TaspStack), NAFlag = "-9999", overwrite = T)

#For T barbigera
TbarList <- list.files(path = "./Layers/Tbarbigera/Trimmed_Tbarbigera", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
TbarStack <- stack(TbarList); #Reads in .asc files as a raster stack
crs(TbarStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers

#Check for correlations
pairs(TbarStack)
reduced_TbarStack <- TbarStack[[-17:-20]]
reduced_TbarbStack <- reduced_TbarStack[[-5:-7]]
reduced_TbarbStack <- reduced_TbarStack[[-2]]
pairs(reduced_TbarStack)
writeRaster(reduced_TbarStack, filename = "./Layers/Tbarbigera/", 
            format = "ascii", bylayer = T, suffix=names(reduced_TbarStack), NAFlag = "-9999", overwrite = T)

#And for T melastomoides
TmelList <- list.files(path = "./Layers/Tmelastomoides/Trimmed_Tmelastomoides", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
TmelStack <- stack(TmelList); #Reads in .asc files as a raster stack
crs(TmelStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers


pairs(TmelStack)
reduced_TmelStack <- TmelStack[[-17:-20]]
reduced_TmelStack <- reduced_TmelStack[[-3:-7]]
pairs(reduced_TmelStack)
writeRaster(reduced_TmelStack, filename = "./Layers/Tmelastomoides/", 
            format = "ascii", bylayer = T, suffix=names(reduced_TmelStack), NAFlag = "-9999", overwrite = T)
