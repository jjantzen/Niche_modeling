#Reducing variables, removing correlated variables
#Remove variables that have a correlation coefficient of >= 0.90.

#BIO1 = Annual Mean Temperature
#BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))
#BIO3 = Isothermality (BIO2/BIO7) (×100)
#BIO4 = Temperature Seasonality (standard deviation ×100)
#BIO5 = Max Temperature of Warmest Month
#BIO6 = Min Temperature of Coldest Month
#BIO7 = Temperature Annual Range (BIO5-BIO6)
#BIO8 = Mean Temperature of Wettest Quarter
#BIO9 = Mean Temperature of Driest Quarter
#BIO10 = Mean Temperature of Warmest Quarter
#BIO11 = Mean Temperature of Coldest Quarter
#BIO12 = Annual Precipitation
#BIO13 = Precipitation of Wettest Month
#BIO14 = Precipitation of Driest Month
#BIO15 = Precipitation Seasonality (Coefficient of Variation)
#BIO16 = Precipitation of Wettest Quarter
#BIO17 = Precipitation of Driest Quarter
#BIO18 = Precipitation of Warmest Quarter
#BIO19 = Precipitation of Coldest Quarter


library(raster)
#Get all environmental and soil layers loaded

NorthList <- list.files(path = "./Layers/3_clades/North", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
NorthStack <- stack(NorthList)
crs(NorthStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers

#Check variables for correlations
pairs(NorthStack[[1:10]], gap=1/10, lower.panel = NULL)
pairs(NorthStack[[11:20]], gap=1/10, lower.panel = NULL)
pairs(NorthStack[[5:15]], gap=1/10, lower.panel = NULL)

#2,4,5,6; 18,19; 8,11; 12,9; 
#alt, bio1,10,11; bio13, bio16; bio14, bio17; bio5 and bio6
#remove 2,5,6; 8, 9, 18
reduced_NorthStack <- NorthStack[[-c(2,5,6,8,9,18)]]
pairs(reduced_NorthStack)
#remove 13, 15, 16; 
reduced_NorthStack2 <- reduced_NorthStack[[-c(13,15,16)]]
pairs(reduced_NorthStack2, gap=1/10, lower.panel = NULL)

writeRaster(reduced_NorthStack2, filename = "./Layers/3_clades/North/Reduced/", 
            format = "ascii", bylayer = T, suffix=names(reduced_NorthStack2), NAFlag = "-9999", overwrite = T)

#######

SouthList <- list.files(path = "./Layers/3_clades/South", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
SouthStack <- stack(SouthList)
crs(SouthStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers

#Check variables for correlations
pairs(SouthStack[[1:10]])
pairs(SouthStack[[11:20]])
#2,4,5,6; 1,7; 19, 20
#remove 2,5,6; 7; 19

reduced_SouthStack <- SouthStack[[-c(2,5,6,7,19)]]

pairs(reduced_SouthStack)
#4,7; 5,6,8; 3, 16, 17; 11, 15
#bio 13 bio 16; bio14, bio15 bio 17; bio1, bio8 bio 9; bio2, bio7
#remove 4, 5, 6, 16, 17, 15

reduced_SouthStack2 <- reduced_SouthStack[[-c(4,5,6,15,16,17)]]

pairs(reduced_SouthStack2)


writeRaster(reduced_SouthStack2, filename = "./Layers/3_clades/South/Reduced/", 
            format = "ascii", bylayer = T, suffix=names(reduced_SouthStack2), NAFlag = "-9999", overwrite = T)

############
WideList <- list.files(path = "./Layers/3_clades/Wide/", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
WideStack <- stack(WideList)
crs(WideStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers

#Check variables for correlations
pairs(WideStack[[1:10]])
pairs(WideStack[[11:20]])
#1,2,3,4; 6,9; 7,10; 13,18; 16, 17, 19, 20
#alt, bio 1, bio 10, bio 11; bio13, bio 16; bio 14, bio 17; bio2, bio7; bio5, bio6, bio8, bio9
#remove 1,3,4; 6; 7; 18; 16, 19, 20
#reduced_WideStack <- WideStack[[-c(1,3,4,6,7,16,18,19,20)]]
reduced_WideStack <- WideStack[[-c(2,5,6,8,9,18,20,21,22)]]

pairs(reduced_WideStack)
reduced_WideStack2 <- reduced_WideStack[[-4]]


pairs(reduced_WideStack2)
names(reduced_WideStack2)
writeRaster(reduced_WideStack2, filename = "./Layers/3_clades/Wide/Reduced/", 
            format = "ascii", bylayer = T, suffix=names(reduced_WideStack2), NAFlag = "-9999", overwrite = T)



#Saved reduced set if you don't want to hunt for the variables you want to get rid of.

