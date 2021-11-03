#Reducing variables, removing correlated variables

library(raster)
#Get all environmental and soil layers loaded


#AegList <- list.files(path = "./Layers/Layers_Maxent_input/Aegopogon", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
AegList <- list.files(path = "./Layers/Layers_Maxent_input/Aegopogon/Reduced", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
AegStack <- stack(AegList)
crs(AegStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers

#Check variables for correlations
pairs(AegStack)

#reduced_AegStack <- var_stack[[-16:-17]]


##Albescens
#AlbList <- list.files(path = "./Layers/Layers_Maxent_input/Albescens", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
AlbList <- list.files(path = "./Layers/Layers_Maxent_input/Albescens/Reduced", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
AlbStack <- stack(AlbList)
crs(AlbStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers

#Check variables for correlations
pairs(AlbStack)

reduced_var_stack <- var_stack[[-16:-17]]

########Angustifolia
#AngustList <- list.files(path = "./Layers/Layers_Maxent_input/Angustifolia", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
AngustList <- list.files(path = "./Layers/Layers_Maxent_input/Angustifolia/Reduced", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
AngustStack <- stack(AngustList)
crs(AngustStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers

#Check variables for correlations
pairs(AngustStack)

reduced_var_stack <- var_stack[[-16:-17]]

#####Aspera
#AspList <- list.files(path = "./Layers/Layers_Maxent_input/Aspera", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
AspList <- list.files(path = "./Layers/Layers_Maxent_input/Aspera/Reduced", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
AspStack <- stack(AspList)
crs(AspStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers

#Check variables for correlations
pairs(AspStack)

reduced_var_stack <- var_stack[[-16:-17]]

####Barbigera
#BarList <- list.files(path = "./Layers/Layers_Maxent_input/Barbigera", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
BarList <- list.files(path = "./Layers/Layers_Maxent_input/Barbigera/Reduced", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
BarStack <- stack(BarList)
crs(BarStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers

#Check variables for correlations
pairs(BarStack)

reduced_var_stack <- var_stack[[-16:-17]]


####Bipenicillata
#BipenList <- list.files(path = "./Layers/Layers_Maxent_input/Bipenicillata", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
BipenList <- list.files(path = "./Layers/Layers_Maxent_input/Bipenicillata/Reduced", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
BipenStack <- stack(BipenList)
crs(BipenStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers

pairs(BipenStack)

reduced_var_stack <- var_stack[[-16:-17]]

#####Dissitiflora
#DissList <- list.files(path = "./Layers/Layers_Maxent_input/Dissitiflora", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
DissList <- list.files(path = "./Layers/Layers_Maxent_input/Dissitiflora/Reduced", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
DissStack <- stack(DissList)
crs(DissStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers

#Check variables for correlations
pairs(DissStack)

reduced_var_stack <- var_stack[[-16:-17]]

#####Duidae
#DuidList <- list.files(path = "./Layers/Layers_Maxent_input/Duidae", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
DuidList <- list.files(path = "./Layers/Layers_Maxent_input/Duidae/Reduced", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
DuidStack <- stack(DuidList)
crs(DuidStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers

pairs(DuidStack)

reduced_var_stack <- var_stack[[-16:-17]]

######Fraterna
#FratList <- list.files(path = "./Layers/Layers_Maxent_input/Fraterna", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
FratList <- list.files(path = "./Layers/Layers_Maxent_input/Fraterna/Reduced", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
FratStack <- stack(FratList)
crs(FratStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers

pairs(FratStack)

reduced_var_stack <- var_stack[[-16:-17]]

#####Llanorum
#LlanList <- list.files(path = "./Layers/Layers_Maxent_input/Llanorum", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
LlanList <- list.files(path = "./Layers/Layers_Maxent_input/Llanorum/Reduced", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
LlanStack <- stack(LlanList)
crs(LlanStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers

pairs(LlanStack)

reduced_var_stack <- var_stack[[-16:-17]]

####Melastomoides
#MelList <- list.files(path = "./Layers/Layers_Maxent_input/Melastomoides", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
MelList <- list.files(path = "./Layers/Layers_Maxent_input/Melastomoides/Reduced", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
MelStack <- stack(MelList)
crs(MelStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers
pairs(MelStack)

reduced_var_stack <- var_stack[[-16:-17]]

####Nigricans
#NigList <- list.files(path = "./Layers/Layers_Maxent_input/Nigricans", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
NigList <- list.files(path = "./Layers/Layers_Maxent_input/Nigricans/Reduced", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
NigStack <- stack(NigList)
crs(NigStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers

pairs(NigStack)

reduced_var_stack <- var_stack[[-16:-17]]

#####Papyrus
#PapList <- list.files(path = "./Layers/Layers_Maxent_input/Papyrus", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
PapList <- list.files(path = "./Layers/Layers_Maxent_input/Papyrus/Reduced", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
PapStack <- stack(PapList)
crs(PapStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers

pairs(PapStack)

reduced_var_stack <- var_stack[[-16:-17]]

#####Verticillaris
#VerList <- list.files(path = "./Layers/Layers_Maxent_input/Verticillaris", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
VerList <- list.files(path = "./Layers/Layers_Maxent_input/Verticillaris/Reduced", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
VerStack <- stack(VerList)
crs(VerStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers

pairs(VerStack)

reduced_var_stack <- var_stack[[-23]]
reduced_var_stack <- reduced_var_stack[[-20]]
reduced_var_stack <- reduced_var_stack[[-19]]
reduced_var_stack <- reduced_var_stack[[-18]]
reduced_var_stack <- reduced_var_stack[[-16]]
reduced_var_stack <- reduced_var_stack[[-8]]
reduced_var_stack <- reduced_var_stack[[-7]]
reduced_var_stack <- reduced_var_stack[[-6]]
reduced_var_stack <- reduced_var_stack[[-5]]
reduced_var_stack <- reduced_var_stack[[-4]]
reduced_var_stack <- reduced_var_stack[[-3]]
reduced_var_stack <- reduced_var_stack[[-1]]

pairs(reduced_var_stack)
names(reduced_var_stack)

#Extract info for points
points_Taeg <- read.csv("./Data/Cleaned_occurrences_by_species/Taegopogon_cleaned_occurrences.csv", stringsAsFactors = FALSE)
#Soil_raster <- raster("./Layers/Taegopogon/Trimmed_Taegopogon/Taegopogon_sndppt_resolution2.asc")
soil <- AegSoilStack[[1]]
soil_raster <- raster(soil)
soil_raster
soil_data_for_points <- extract(soil_raster, points_Taeg[,2:3])
soil_data_for_points
nrow(points_Taeg)
points_Taeg
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
compareRaster(AegEnvStack, AegSoilStack)

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



bio1 <- raster("./Layers/Layers_Maxent_input/Aegopogon/Aegopogon_bio1.asc")
bdticm <- raster("./Layers/Layers_Maxent_input/Aegopogon/Aegopogon_bdticm.asc")
compareRaster(bio1, bdticm)
bio1
bdticm
