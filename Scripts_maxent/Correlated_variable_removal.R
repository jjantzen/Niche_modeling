#Reducing variables, removing correlated variables

library(raster)
#Get all environmental and soil layers loaded


AegSoilList <- list.files(path = "./Layers/Trimmed_soil/Aegopogon", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
AegSoilList

AegSoilStack <- stack(AegSoilList)

extent(AegSoilStack)
res(AegSoilStack)
crs(AegSoilStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers

AegEnvList <- list.files(path = "./Layers/Trimmed_worldclim/Aegopogon/Cropped", pattern = ".asc", full.names = TRUE)
AegEnvStack <- stack(AegEnvList)
extent(AegEnvStack)
crs(AegEnvStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers


#Match envt extent to match soil extents
extent(AegSoilStack)
extent(AegEnvStack)

compareRaster(AegEnvStack, AegSoilStack)

#Combine soil and evnt into one stack

var_stack <- stack(AegEnvStack, AegSoilStack)

names(var_stack)

#Check variables for correlations
pairs(var_stack)
pairs(AegSoilStack)

env_1 <- AegEnvStack[[1:10]]
env_2 <- AegEnvStack[[11:20]]
env_2
pairs(env_2)
pairs(AegEnvStack)

names(var_stack)

reduced_var_stack <- var_stack[[-16:-17]]


##Albescens
AlbSoilList <- list.files(path = "./Layers/Trimmed_soil/Albescens", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
AlbSoilList

AlbSoilStack <- stack(AlbSoilList)

extent(AlbSoilStack)
res(AlbSoilStack)
crs(AlbSoilStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers

AlbEnvList <- list.files(path = "./Layers/Trimmed_worldclim/Albescens/Cropped", pattern = ".asc", full.names = TRUE)
AlbEnvStack <- stack(AlbEnvList)
extent(AlbEnvStack)
crs(AlbEnvStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers


#Match envt extent to match soil extents
extent(AlbSoilStack)
extent(AlbEnvStack)

compareRaster(AlbEnvStack, AlbSoilStack)

#Combine soil and evnt into one stack

var_stack <- stack(AlbEnvStack, AlbSoilStack)

names(var_stack)

#Check variables for correlations
pairs(var_stack)

reduced_var_stack <- var_stack[[-16:-17]]

env_1 <- AlbEnvStack[[1:10]]
env_2 <- AlbEnvStack[[11:20]]

pairs(env_1)
pairs(env_2)
pairs(AlbSoilStack)


########Angustifolia
AngustSoilList <- list.files(path = "./Layers/Trimmed_soil/Angustifolia", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
AngustSoilList

AngustSoilStack <- stack(AngustSoilList)

extent(AngustSoilStack)
res(AngustSoilStack)
crs(AngustSoilStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers

AngustEnvList <- list.files(path = "./Layers/Trimmed_worldclim/Angustifolia/Cropped", pattern = ".asc", full.names = TRUE)
AngustEnvStack <- stack(AngustEnvList)
extent(AngustEnvStack)
crs(AngustEnvStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers


#Match envt extent to match soil extents
extent(AngustSoilStack)
extent(AngustEnvStack)

compareRaster(AngustEnvStack, AngustSoilStack)

#Combine soil and evnt into one stack

var_stack <- stack(AngustEnvStack, AngustSoilStack)

names(var_stack)

#Check variables for correlations
pairs(var_stack)
env_1 <- AngustEnvStack[[1:10]]
env_2 <- AngustEnvStack[[11:20]]

pairs(env_1)
pairs(env_2)
pairs(AngustSoilStack)

reduced_var_stack <- var_stack[[-16:-17]]

#####Aspera
AspSoilList <- list.files(path = "./Layers/Trimmed_soil/Aspera", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
AspSoilList

AspSoilStack <- stack(AspSoilList)

extent(AspSoilStack)
res(AspSoilStack)
crs(AspSoilStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers

AspEnvList <- list.files(path = "./Layers/Trimmed_worldclim/Aspera/Cropped", pattern = ".asc", full.names = TRUE)
AspEnvStack <- stack(AspEnvList)
extent(AspEnvStack)
crs(AspEnvStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers


#Match envt extent to match soil extents
extent(AspSoilStack)
extent(AspEnvStack)

compareRaster(AspEnvStack, AspSoilStack)

#Combine soil and evnt into one stack

var_stack <- stack(AspEnvStack, AspSoilStack)

names(var_stack)

#Check variables for correlations
pairs(var_stack)
env_1 <- AspEnvStack[[1:10]]
env_2 <- AspEnvStack[[11:20]]

pairs(env_1)
pairs(env_2)
pairs(AspSoilStack)

reduced_var_stack <- var_stack[[-16:-17]]

####Barbigera
BarSoilList <- list.files(path = "./Layers/Trimmed_soil/Barbigera", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
BarSoilList

BarSoilStack <- stack(BarSoilList)

extent(BarSoilStack)
res(BarSoilStack)
crs(BarSoilStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers

BarEnvList <- list.files(path = "./Layers/Trimmed_worldclim/Barbigera/Cropped", pattern = ".asc", full.names = TRUE)
BarEnvStack <- stack(BarEnvList)
extent(BarEnvStack)
crs(BarEnvStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers


#Match envt extent to match soil extents
extent(BarSoilStack)
extent(BarEnvStack)

compareRaster(BarEnvStack, BarSoilStack)

#Combine soil and evnt into one stack

var_stack <- stack(BarEnvStack, BarSoilStack)

names(var_stack)

#Check variables for correlations
pairs(var_stack)
env_1 <- BarEnvStack[[1:10]]
env_2 <- BarEnvStack[[11:20]]

pairs(env_1)
pairs(env_2)
pairs(BarSoilStack)

reduced_var_stack <- var_stack[[-16:-17]]


####Bipenicillata
BipenSoilList <- list.files(path = "./Layers/Trimmed_soil/Bipenicillata", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
BipenSoilList

BipenSoilStack <- stack(BipenSoilList)

extent(BipenSoilStack)
res(BipenSoilStack)
crs(BipenSoilStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers

BipenEnvList <- list.files(path = "./Layers/Trimmed_worldclim/Bipenicillata/Cropped", pattern = ".asc", full.names = TRUE)
BipenEnvStack <- stack(BipenEnvList)
extent(BipenEnvStack)
crs(BipenEnvStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers


#Match envt extent to match soil extents
extent(BipenSoilStack)
extent(BipenEnvStack)

compareRaster(BipenEnvStack, BipenSoilStack)

#Combine soil and evnt into one stack

var_stack <- stack(BipenEnvStack, BipenSoilStack)

names(var_stack)

#Check variables for correlations
pairs(var_stack)
env_1 <- BipenEnvStack[[1:10]]
env_2 <- BipenEnvStack[[11:20]]

pairs(env_1)
pairs(env_2)
pairs(BipenSoilStack)

reduced_var_stack <- var_stack[[-16:-17]]

#####Dissitiflora
DissSoilList <- list.files(path = "./Layers/Trimmed_soil/Dissitiflora", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
DissSoilList

DissSoilStack <- stack(DissSoilList)

extent(DissSoilStack)
res(DissSoilStack)
crs(DissSoilStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers

DissEnvList <- list.files(path = "./Layers/Trimmed_worldclim/Dissitiflora/Cropped", pattern = ".asc", full.names = TRUE)
DissEnvStack <- stack(DissEnvList)
extent(DissEnvStack)
crs(DissEnvStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers


#Match envt extent to match soil extents
extent(DissSoilStack)
extent(DissEnvStack)

compareRaster(DissEnvStack, DissSoilStack)

#Combine soil and evnt into one stack

var_stack <- stack(DissEnvStack, DissSoilStack)

names(var_stack)

#Check variables for correlations
pairs(var_stack)
env_1 <- DissEnvStack[[1:10]]
env_2 <- DissEnvStack[[11:20]]

pairs(env_1)
pairs(env_2)
pairs(DissSoilStack)

reduced_var_stack <- var_stack[[-16:-17]]

#####Duidae
DuidSoilList <- list.files(path = "./Layers/Trimmed_soil/Duidae", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
DuidSoilList

DuidSoilStack <- stack(DuidSoilList)

extent(DuidSoilStack)
res(DuidSoilStack)
crs(DuidSoilStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers

DuidEnvList <- list.files(path = "./Layers/Trimmed_worldclim/Duidae/Cropped", pattern = ".asc", full.names = TRUE)
DuidEnvStack <- stack(DuidEnvList)
extent(DuidEnvStack)
crs(DuidEnvStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers


#Match envt extent to match soil extents
extent(DuidSoilStack)
extent(DuidEnvStack)

compareRaster(DuidEnvStack, DuidSoilStack)

#Combine soil and evnt into one stack

var_stack <- stack(DuidEnvStack, DuidSoilStack)

names(var_stack)

#Check variables for correlations
pairs(var_stack)
env_1 <- DuidEnvStack[[1:10]]
env_2 <- DuidEnvStack[[11:20]]

pairs(env_1)
pairs(env_2)
pairs(DuidSoilStack)

reduced_var_stack <- var_stack[[-16:-17]]

######Fraterna
FratSoilList <- list.files(path = "./Layers/Trimmed_soil/Fraterna", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
FratSoilList

FratSoilStack <- stack(FratSoilList)

extent(FratSoilStack)
res(FratSoilStack)
crs(FratSoilStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers

FratEnvList <- list.files(path = "./Layers/Trimmed_worldclim/Fraterna/Cropped", pattern = ".asc", full.names = TRUE)
FratEnvStack <- stack(FratEnvList)
extent(FratEnvStack)
crs(FratEnvStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers


#Match envt extent to match soil extents
extent(FratSoilStack)
extent(FratEnvStack)

compareRaster(FratEnvStack, FratSoilStack)

#Combine soil and evnt into one stack

var_stack <- stack(FratEnvStack, FratSoilStack)

names(var_stack)

#Check variables for correlations
pairs(var_stack)
env_1 <- FratEnvStack[[1:10]]
env_2 <- FratEnvStack[[11:20]]

pairs(env_1)
pairs(env_2)
pairs(FratSoilStack)

reduced_var_stack <- var_stack[[-16:-17]]

#####Llanorum
LlanSoilList <- list.files(path = "./Layers/Trimmed_soil/Llanorum", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
LlanSoilList

LlanSoilStack <- stack(LlanSoilList)

extent(LlanSoilStack)
res(LlanSoilStack)
crs(LlanSoilStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers

LlanEnvList <- list.files(path = "./Layers/Trimmed_worldclim/Llanorum/Cropped", pattern = ".asc", full.names = TRUE)
LlanEnvStack <- stack(LlanEnvList)
extent(LlanEnvStack)
crs(LlanEnvStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers


#Match envt extent to match soil extents
extent(LlanSoilStack)
extent(LlanEnvStack)

compareRaster(LlanEnvStack, LlanSoilStack)

#Combine soil and evnt into one stack

var_stack <- stack(LlanEnvStack, LlanSoilStack)

names(var_stack)

#Check variables for correlations
pairs(var_stack)
env_1 <- LlanEnvStack[[1:10]]
env_2 <- LlanEnvStack[[11:20]]

pairs(env_1)
pairs(env_2)
pairs(LlanSoilStack)

reduced_var_stack <- var_stack[[-16:-17]]

####Melastomoides
MelSoilList <- list.files(path = "./Layers/Trimmed_soil/Melastomoides", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
MelSoilList

MelSoilStack <- stack(MelSoilList)

extent(MelSoilStack)
res(MelSoilStack)
crs(MelSoilStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers

MelEnvList <- list.files(path = "./Layers/Trimmed_worldclim/Melastomoides/Cropped", pattern = ".asc", full.names = TRUE)
MelEnvStack <- stack(MelEnvList)
extent(MelEnvStack)
crs(MelEnvStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers


#Match envt extent to match soil extents
extent(MelSoilStack)
extent(MelEnvStack)

compareRaster(MelEnvStack, MelSoilStack)

#Combine soil and evnt into one stack

var_stack <- stack(MelEnvStack, MelSoilStack)

names(var_stack)

#Check variables for correlations
pairs(var_stack)
env_1 <- MelEnvStack[[1:10]]
env_2 <- MelEnvStack[[11:20]]

pairs(env_1)
pairs(env_2)
pairs(MelSoilStack)

reduced_var_stack <- var_stack[[-16:-17]]

####Nigricans
NigSoilList <- list.files(path = "./Layers/Trimmed_soil/Nigricans", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
NigSoilList

NigSoilStack <- stack(NigSoilList)

extent(NigSoilStack)
res(NigSoilStack)
crs(NigSoilStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers

NigEnvList <- list.files(path = "./Layers/Trimmed_worldclim/Nigricans/Cropped", pattern = ".asc", full.names = TRUE)
NigEnvStack <- stack(NigEnvList)
extent(NigEnvStack)
crs(NigEnvStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers


#Match envt extent to match soil extents
extent(NigSoilStack)
extent(NigEnvStack)

compareRaster(NigEnvStack, NigSoilStack)

#Combine soil and evnt into one stack

var_stack <- stack(NigEnvStack, NigSoilStack)

names(var_stack)

#Check variables for correlations
pairs(var_stack)
env_1 <- NigEnvStack[[1:10]]
env_2 <- NigEnvStack[[11:20]]

pairs(env_1)
pairs(env_2)
pairs(NigSoilStack)

reduced_var_stack <- var_stack[[-16:-17]]

#####Papyrus
PapSoilList <- list.files(path = "./Layers/Trimmed_soil/Papyrus", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
PapSoilList

PapSoilStack <- stack(PapSoilList)

extent(PapSoilStack)
res(PapSoilStack)
crs(PapSoilStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers

PapEnvList <- list.files(path = "./Layers/Trimmed_worldclim/Papyrus/Cropped", pattern = ".asc", full.names = TRUE)
PapEnvStack <- stack(PapEnvList)
extent(PapEnvStack)
crs(PapEnvStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers


#Match envt extent to match soil extents
extent(PapSoilStack)
extent(PapEnvStack)

compareRaster(PapEnvStack, PapSoilStack)

#Combine soil and evnt into one stack

var_stack <- stack(PapEnvStack, PapSoilStack)

names(var_stack)

#Check variables for correlations
pairs(var_stack)
env_1 <- PapEnvStack[[1:10]]
env_2 <- PapEnvStack[[11:20]]

pairs(env_1)
pairs(env_2)
pairs(PapSoilStack)

reduced_var_stack <- var_stack[[-16:-17]]

#####Verticillaris
VerSoilList <- list.files(path = "./Layers/Trimmed_soil/Verticillaris", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
VerSoilList

VerSoilStack <- stack(VerSoilList)

extent(VerSoilStack)
res(VerSoilStack)
crs(VerSoilStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers

VerEnvList <- list.files(path = "./Layers/Trimmed_worldclim/Verticillaris/Cropped", pattern = ".asc", full.names = TRUE)
VerEnvStack <- stack(VerEnvList)
extent(VerEnvStack)
crs(VerEnvStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers


#Match envt extent to match soil extents
extent(VerSoilStack)
extent(VerEnvStack)

compareRaster(VerEnvStack, VerSoilStack)

#Combine soil and evnt into one stack

var_stack <- stack(VerEnvStack, VerSoilStack)

names(var_stack)

#Check variables for correlations
pairs(var_stack)
env_1 <- VerEnvStack[[1:10]]
env_2 <- VerEnvStack[[11:20]]

pairs(env_1)
pairs(env_2)
pairs(VerSoilStack)

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
