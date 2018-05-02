#Lab 9 Helper script, post-processing models and using BIOMOD
#========================================================
#author: Hannah Owens
#date: 1 March, 2017

#Set the working directory
#====================================

###FOR REQUIRED PART OF LAB
library(phyloclim);
#Calculating PNO
library(hypervolume);
#Analyze niche as an n-dimensional hypervolume
library(raster);
#Always
library(RStoolbox);
library(rgdal)
#To do PCAs of rasters for niche comparisons

library(ggplot2)

####FOR OPTIONAL PART OF LAB
library(raster);
library(rgdal);
library(biomod2);
library(pROC);

#LAB 9 REQUIRED SECTION: SOME MODEL POST-PROCESSING JOLLITY   ####
##################################################################

#Get environmental data used to train your model

Taeg_envtList <- list.files(path = "./Layers/Taegopogon/Trimmed_Taegopogon", pattern = ".asc", full.names=TRUE)
Taeg_envtStack <- stack(Taeg_envtList);

Tasp_envtList <- list.files(path = "./Layers/Taspera/Small_Taspera", pattern = ".asc", full.names = TRUE)
Tasp_envtStack <- stack(Tasp_envtList)

Tbar_envtList <- list.files(path = "./Layers/Tbarbigera/Trimmed_Tbarbigera", pattern = ".asc", full.names = TRUE)
Tbar_envtStack <- stack(Tbar_envtList)

Tmel_envtList <- list.files(path = "./Layers/Tmelastomoides/Trimmed_Tmelastomoides", pattern = ".asc", full.names = TRUE)
Tmel_envtStack <- stack(Tmel_envtList)

proj_envtList <- list.files(path = "./Data/NW/Taegopogon/", pattern = ".asc", full.names=TRUE)
proj_envtStack <- stack(proj_envtList)

#Get occurrence points
Taeg_Points <- read.csv("./Data/clean_Taeg_PointsSPOCC.csv")
Tasp_Points <- read.csv("./Data/clean_Tasp_PointsSPOCC_no_outlier.csv");
Tbar_Points <- read.csv("./Data/clean_Tbar_PointsSPOCC.csv")
Tmel_Points <- read.csv("./Data/clean_Tmel_PointsSPOCC.csv")
# Thresholding
#====================================

# Example thresholding to 95% suitabilty score. You can use any number, though.
Taeg_model <- raster("./Output/Taegopogon/Most_no_soil/Tibouchina_aegopogon_NW_avg.asc");
suitabilityScores <- extract(Taeg_model, Taeg_Points[,2:3]); #Extract suitability scores at occurrences
suitabilityScores <- suitabilityScores[complete.cases(suitabilityScores)] #Clean out no data values
threshold <- sort(suitabilityScores, decreasing = T)[round(length(suitabilityScores)*.95,0)] #Takes 5th percentile suitability score
m <- c(0, threshold, 0,  threshold, 1, 1); #Create a list of thresholds in the following format: (Minimum, maximum, valueToChange)
rclmat <- matrix(m, ncol=3, byrow=TRUE); #Changes the threshold list to a matrix
Taeg_Dist <- reclassify(Taeg_model, rcl = rclmat); #Reclassify suitability surface to presence/absence

#NOTE: codDist can be saved like any other raster file (refer to Lab 2);
writeRaster(Taeg_Dist, "./Output/Taeg_Dist", format = "ascii", overwrite = TRUE);

#Second species
Tasp_model <- raster("./Output/Taspera/Most_no_soil/Tibouchina_aspera_NW_avg.asc");
suitabilityScores <- extract(Tasp_model, Tasp_Points[,2:3]); #Extract suitability scores at occurrences
suitabilityScores <- suitabilityScores[complete.cases(suitabilityScores)] #Clean out no data values
threshold <- sort(suitabilityScores, decreasing = T)[round(length(suitabilityScores)*.95,0)] #Takes 5th percentile suitability score
m <- c(0, threshold, 0,  threshold, 1, 1); #Create a list of thresholds in the following format: (Minimum, maximum, valueToChange)
rclmat <- matrix(m, ncol=3, byrow=TRUE); #Changes the threshold list to a matrix
Tasp_Dist <- reclassify(Tasp_model, rcl = rclmat); #Reclassify suitability surface to presence/absence
#NOTE: codDist can be saved like any other raster file (refer to Lab 2);
writeRaster(Tasp_Dist, "./Output/Tasp_Dist", format = "ascii", overwrite = TRUE);

#Second species
Tbar_model <- raster("./Output/Tbarbigera/Most_no_soil/Tibouchina_barbigera_NW_avg.asc");
suitabilityScores <- extract(Tbar_model, Tbar_Points[,2:3]); #Extract suitability scores at occurrences
suitabilityScores <- suitabilityScores[complete.cases(suitabilityScores)] #Clean out no data values
threshold <- sort(suitabilityScores, decreasing = T)[round(length(suitabilityScores)*.95,0)] #Takes 5th percentile suitability score
m <- c(0, threshold, 0,  threshold, 1, 1); #Create a list of thresholds in the following format: (Minimum, maximum, valueToChange)
rclmat <- matrix(m, ncol=3, byrow=TRUE); #Changes the threshold list to a matrix
Tbar_Dist <- reclassify(Tbar_model, rcl = rclmat); #Reclassify suitability surface to presence/absence
#NOTE: codDist can be saved like any other raster file (refer to Lab 2);
writeRaster(Tbar_Dist, "./Output/Tbar_Dist", format = "ascii", overwrite = TRUE);

#Second species
Tmel_model <- raster("./Output/Tmelastomoides/Most_no_soil/Tibouchina_melastomoides_NW_avg.asc");
suitabilityScores <- extract(Tmel_model, Tmel_Points[,2:3]); #Extract suitability scores at occurrences
suitabilityScores <- suitabilityScores[complete.cases(suitabilityScores)] #Clean out no data values
threshold <- sort(suitabilityScores, decreasing = T)[round(length(suitabilityScores)*.95,0)] #Takes 5th percentile suitability score
m <- c(0, threshold, 0,  threshold, 1, 1); #Create a list of thresholds in the following format: (Minimum, maximum, valueToChange)
rclmat <- matrix(m, ncol=3, byrow=TRUE); #Changes the threshold list to a matrix
Tmel_Dist <- reclassify(Tmel_model, rcl = rclmat); #Reclassify suitability surface to presence/absence
#NOTE: codDist can be saved like any other raster file (refer to Lab 2);
writeRaster(Tmel_Dist, "./Output/Tmel_Dist", format = "ascii", overwrite = TRUE);



# Calculating the extent of ENM-suitable habitat
#====================================

#Calculating the extent of ENM-suitable habitat
cellAreas <- area(Taeg_Dist)*Taeg_Dist #Makes a raster of cell area
plot(cellAreas);
cellAreaMeasures <- rasterToPoints(cellAreas); #Adds up the area of each cell
Taeg_area <- paste("The area of Tibouchina aegopogon's distribution is ", sum(cellAreaMeasures[,3]), " km2.", sep=""); #prints an answer

cellAreas <- area(Tasp_Dist)*Tasp_Dist #Makes a raster of cell area
plot(cellAreas);
cellAreaMeasures <- rasterToPoints(cellAreas); #Adds up the area of each cell
Tasp_area <- paste("The area of Tibouchina aspera's distribution is ", sum(cellAreaMeasures[,3]), " km2.", sep=""); #prints an answer
Tasp_area

cellAreas <- area(Tbar_Dist)*Tbar_Dist #Makes a raster of cell area
plot(cellAreas);
cellAreaMeasures <- rasterToPoints(cellAreas); #Adds up the area of each cell
Tbar_area <- paste("The area of Tibouchina barbigera's distribution is ", sum(cellAreaMeasures[,3]), " km2.", sep=""); #prints an answer
Tbar_area

cellAreas <- area(Tmel_Dist)*Tmel_Dist #Makes a raster of cell area
plot(cellAreas);
cellAreaMeasures <- rasterToPoints(cellAreas); #Adds up the area of each cell
Tmel_area <- paste("The area of Tibouchina melastomoides's distribution is ", sum(cellAreaMeasures[,3]), " km2.", sep=""); #prints an answer
Tmel_area



#Basic niche characterization
#====================================

# Extracting descriptive statistics from ENM-predicted presences
#T aegopogon
Taeg_Suit <- Taeg_envtStack * Taeg_Dist;
names(Taeg_Suit) <- names(Taeg_envtStack);
Taeg_Pts <- cbind(Taeg_Points, extract(Taeg_envtStack, Taeg_Points[,2:3]));
Taeg_Pts <- Taeg_Pts[complete.cases(Taeg_Pts),]
write.csv(Taeg_Pts, "./Output/Taeg_Pts.csv")
#Shows difference in inferred niche characteristics
cellStats(Taeg_envtStack, stat = 'max'); #Characteristics across training region
cellStats(Taeg_Suit, stat = 'max'); #Characteristics at ENM-predicted presences
noquote(summary(Taeg_Pts[,5:9], digits = 2)[6,]); #Characteristics at occurrence points
writeRaster(Taeg_Suit, "./Output/Taeg_Suit", format = "ascii", overwrite = TRUE, bylayer = TRUE, suffix=names(Taeg_Suit));

#T aspera
Tasp_Suit <- Tasp_envtStack * Tasp_Dist;
names(Tasp_Suit) <- names(Tasp_envtStack);
Tasp_Pts <- cbind(Tasp_Points, extract(Tasp_envtStack, Tasp_Points[,2:3]));
Tasp_Pts <- Tasp_Pts[complete.cases(Tasp_Pts),]
write.csv(Tasp_Pts, "./Output/Tasp_Pts.csv")
#Shows difference in inferred niche characteristics
#cellStats(Tasp_envtStack, stat = 'max'); #Characteristics across training region
#cellStats(Tasp_Suit, stat = 'max'); #Characteristics at ENM-predicted presences
#noquote(summary(Tasp_Pts[,5:9], digits = 2)[6,]); #Characteristics at occurrence points
writeRaster(Tasp_Suit, "./Output/Tasp_Suit", format = "ascii", overwrite = TRUE, bylayer = TRUE, suffix=names(Tasp_Suit));

#T barbigera
Tbar_Suit <- Tbar_envtStack * Tbar_Dist;
names(Tbar_Suit) <- names(Tbar_envtStack);
Tbar_Pts <- cbind(Tbar_Points, extract(Tbar_envtStack, Tbar_Points[,2:3]));
Tbar_Pts <- Tbar_Pts[complete.cases(Tbar_Pts),]
write.csv(Tbar_Pts, "./Output/Tbar_Pts.csv")
#Shows difference in inferred niche characteristics
#cellStats(Tbar_envtStack, stat = 'max'); #Characteristics across training region
#cellStats(Tbar_Suit, stat = 'max'); #Characteristics at ENM-predicted presences
#noquote(summary(Tbar_Pts[,5:9], digits = 2)[6,]); #Characteristics at occurrence points
writeRaster(Tbar_Suit, "./Output/Tbar_Suit", format = "ascii", overwrite = TRUE, bylayer = TRUE, suffix=names(Tbar_Suit));

#T melastomoides
Tmel_Suit <- Tmel_envtStack * Tmel_Dist;
names(Tmel_Suit) <- names(Tmel_envtStack);
Tmel_Pts <- cbind(Tmel_Points, extract(Tmel_envtStack, Tmel_Points[,2:3]));
Tmel_Pts <- Tmel_Pts[complete.cases(Tmel_Pts),]
write.csv(Tmel_Pts, "./Output/Tmel_Pts.csv")
#Shows difference in inferred niche characteristics
#cellStats(Tmel_envtStack, stat = 'max'); #Characteristics across training region
#cellStats(Tmel_Suit, stat = 'max'); #Characteristics at ENM-predicted presences
#noquote(summary(Tmel_Pts[,5:9], digits = 2)[6,]); #Characteristics at occurrence points
writeRaster(Tmel_Suit, "./Output/Tmel_Suit", format = "ascii", overwrite = TRUE, bylayer = TRUE, suffix=names(Tmel_Suit));

#Niche occupancy profiles
#====================================

#Calculate niche occupancy from environmental data and niche models

#Save environmental layers with right NA values
pno_envtList <- list.files(path = "./Output/Post_processing/Unreduced_environment/", pattern = ".asc", full.names=TRUE)
pno_envtStack<- stack(pno_envtList)
writeRaster(pno_envtStack, "./Output/Post_processing/Unreduced_environment/", format = "ascii", overwrite = TRUE, bylayer = TRUE, suffix=names(pno_envtStack), NAFlag = "-9999");
head(pno_envtStack)

#Resample environmental layers
#Taeg_alt_forpno <- raster("./Data/NW/Taegopogon_alt.asc")
#Taeg_model_forpno <- raster("./Output/Post_processing/For_pnos/Other/Tibouchina_aegopogon_NW_avg.asc")
#Tasp_model_forpno <- raster("./Output/Post_processing/For_pnos/Other/Tibouchina_aspera_NW_avg.asc")
#Tbar_model_forpno <- raster("./Output/Post_processing/For_pnos/Other/Tibouchina_barbigera_NW_avg.asc")
#Tmel_model_forpno <- raster("./Output/Post_processing/For_pnos/Other/Tibouchina_melastomoides_NW_avg.asc")

#resampled_proj_envtStack <- resample(proj_envtStack, Taeg_model_forpno, method = "bilinear")
#writeRaster(resampled_proj_envtStack, "./Output/Post_processing/Environment/", format = "ascii", overwrite = TRUE, bylayer = TRUE, suffix=names(resampled_proj_envtStack), NAFlag = "-9999");

#extent(Taeg_alt_forpno)
#extent(Taeg_model_forpno)
#res(Taeg_alt_forpno)
#res(Taeg_model_forpno)
#plot(Taeg_model_forpno)
#plot(Taeg_alt_forpno)
#plot(proj_envtStack)
#crs(NW) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
#crs(Taeg_model_forpno) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

#compareRaster(Tasp_model_forpno, Tmel_model_forpno)
#resample_alt<- resample(Taeg_alt_forpno, Taeg_model_forpno, method="bilinear")
#writeRaster(resample_alt, "./Output/Post_processing/Environment/resampled_alt", format = "ascii", overwrite = TRUE)

#compareRaster(resample_alt, Taeg_model_forpno)
#plot(Taeg_elevPNO)
#Make sure extents not rounded, tabs not spaces, and same capitalization in model and layers
#ALso check which file in OTher
Taeg_elevPNO <- pno(path_bioclim = "./Output/Post_processing/Unreduced_environment/_Taegopogon_alt.asc", path_model = "./Output/Post_processing/For_pnos/Other/", bin_number = 50); 
Tasp_elevPNO <- pno(path_bioclim = "./Output/Post_processing/Unreduced_environment/_Taegopogon_alt.asc", path_model = "./Output/Post_processing/For_pnos/Other/")
#Why are the plots different than they were? No colour, how to divide by 
plot(Taeg_elevPNO)
plot(Tasp_elevPNO)
Taeg_elevPNO
Taeg_alt_test <- raster("./Output/Post_processing/Environment/_Taegopogon_alt.asc")

Taeg_model_test <- raster("./Output/Post_processing/For_pnos/Other/Tibouchina_aegopogon_NW_avg.asc")
compareRaster(Taeg_alt_test, Taeg_model_test)
res(Taeg_alt_test)
res(Taeg_model_test)

Taeg_elevPNO <- pno(path_bioclim = "./Output/Post_processing/Unreduced_environment/_Taegopogon_alt.asc", path_model = "./Output/Post_processing/For_pnos/Other/", bin_number = 50); 
Tasp_elevPNO <- pno(path_bioclim = "./Output/Post_processing/Unreduced_environment/_Taegopogon_alt.asc", path_model = "./Output/Post_processing/For_pnos/Other/", bin_number = 50)
Tbar_elevPNO <- pno(path_bioclim = "./Output/Post_processing/Unreduced_environment/_Taegopogon_alt.asc", path_model = "./Output/Post_processing/For_pnos/Other/", bin_number = 50)
Tmel_elevPNO <- pno(path_bioclim = "./Output/Post_processing/Unreduced_environment/_Taegopogon_alt.asc", path_model = "./Output/Post_processing/For_pnos/Other/", bin_number = 50)

plot(Taeg_elevPNO)
plot(Tasp_elevPNO)
plot(Tbar_elevPNO)
plot(Tmel_elevPNO)

ggplot


Taeg_ann_pptPNO <- pno(path_bioclim = "./Output/Post_processing/Unreduced_environment/_Taegopogon_bio12.asc", path_model = "./Output/Post_processing/For_pnos/Other/", bin_number = 10); 
Tasp_ann_pptPNO <- pno(path_bioclim = "./Output/Post_processing/Unreduced_environment/_Taegopogon_bio12.asc", path_model = "./Output/Post_processing/For_pnos/Other/", bin_number = 10)
Tbar_ann_pptPNO <- pno(path_bioclim = "./Output/Post_processing/Unreduced_environment/_Taegopogon_bio12.asc", path_model = "./Output/Post_processing/For_pnos/Other/", bin_number = 10)
Tmel_ann_pptPNO <- pno(path_bioclim = "./Output/Post_processing/Unreduced_environment/_Taegopogon_bio12.asc", path_model = "./Output/Post_processing/For_pnos/Other/", bin_number = 10)

Taeg_ppt_dryPNO <- pno(path_bioclim = "./Output/Post_processing/Unreduced_environment/_Taegopogon_bio14.asc", path_model = "./Output/Post_processing/For_pnos/Other/", bin_number = 10); 
Tasp_ppt_dryPNO <- pno(path_bioclim = "./Output/Post_processing/Unreduced_environment/_Taegopogon_bio14.asc", path_model = "./Output/Post_processing/For_pnos/Other/", bin_number = 10)
Tbar_ppt_dryPNO <- pno(path_bioclim = "./Output/Post_processing/Unreduced_environment/_Taegopogon_bio14.asc", path_model = "./Output/Post_processing/For_pnos/Other/", bin_number = 10)
Tmel_ppt_dryPNO <- pno(path_bioclim = "./Output/Post_processing/Unreduced_environment/_Taegopogon_bio14.asc", path_model = "./Output/Post_processing/For_pnos/Other/", bin_number = 10)

Taeg_isoPNO <- pno(path_bioclim = "./Output/Post_processing/Unreduced_environment/_Taegopogon_bio3.asc", path_model = "./Output/Post_processing/For_pnos/Other/", bin_number = 10); 
Tasp_isoPNO <- pno(path_bioclim = "./Output/Post_processing/Unreduced_environment/_Taegopogon_bio3.asc", path_model = "./Output/Post_processing/For_pnos/Other/", bin_number = 10)
Tbar_isoPNO <- pno(path_bioclim = "./Output/Post_processing/Unreduced_environment/_Taegopogon_bio3.asc", path_model = "./Output/Post_processing/For_pnos/Other/", bin_number = 10)
Tmel_isoPNO <- pno(path_bioclim = "./Output/Post_processing/Unreduced_environment/_Taegopogon_bio3.asc", path_model = "./Output/Post_processing/For_pnos/Other/", bin_number = 10)

Taeg_mdrPNO <- pno(path_bioclim = "./Output/Post_processing/Unreduced_environment/_Taegopogon_bio2.asc", path_model = "./Output/Post_processing/For_pnos/Other/", bin_number = 10); 
Tasp_mdrPNO <- pno(path_bioclim = "./Output/Post_processing/Unreduced_environment/_Taegopogon_bio2.asc", path_model = "./Output/Post_processing/For_pnos/Other/", bin_number = 10)
Tbar_mdrPNO <- pno(path_bioclim = "./Output/Post_processing/Unreduced_environment/_Taegopogon_bio2.asc", path_model = "./Output/Post_processing/For_pnos/Other/", bin_number = 10)
Tmel_mdrPNO <- pno(path_bioclim = "./Output/Post_processing/Unreduced_environment/_Taegopogon_bio2.asc", path_model = "./Output/Post_processing/For_pnos/Other/", bin_number = 10)

#Plot Niche occupancy profiles

#make jpegs?
plotPNO(tAvgPNO)

;
temp <- raster("../Niche_modeling_lab/Lab9/MyData/Reduced/reduced_Taegopogon_Taegopogon_bio1.asc")
model <- raster("../Niche_modeling_lab/Lab9/MyData/Tibouchina_aegopogon_0_NW.asc")
plot(temp)
extent(model)
extent(temp)
res(model)
res(temp)

re_temp<- resample(temp, model, method="bilinear")
writeRaster(re_temp, "./Lab9/res_temp", format = "ascii")
#Hypervolume overlap
#====================================
#Calculate principle components to render environmental variables comparable
envtPC <- rasterPCA(proj_envtStack)$map #Do principle components to reduce skew
plot(envtStack)
plot(envtPC)
#Create first hypervolume
Taeg_occ <- rasterToPoints(raster("./Output/Post_processing/Taegopogon/Taeg_Dist.asc"))#Need predicted occurrence points (calculated from thresholded model)
Taeg_Envt <- extract(envtPC, Taeg_occ[,1:2]);
head(Taeg_occ)
Taeg_Envt <- Taeg_Envt[complete.cases(Taeg_Envt),];
Taeg_Hyp <- hypervolume(Taeg_Envt, repsperpoint = 5, bandwidth = 1, quantile = .1, name = "Tibouchina aegopogon"); #Calculates hypervolume
plot(Taeg_Hyp)
Taeg_Hyp
#Create second hypervolume
Tasp_occ <- rasterToPoints(raster("./Output/Post_processing/Taspera/Tasp_Dist.asc"))#Getting a second species for comparison
Tasp_Envt <- extract(envtPC, Tasp_occ[,1:2])
Tasp_Envt <- Tasp_Envt[complete.cases(Tasp_Envt),];
Tasp_Hyp <- hypervolume(Tasp_Envt, repsperpoint = 5, bandwidth = 1, quantile = .1, name = "Tibouchina aspera"); #Calculates hypervolume
Tasp_Hyp

#Third hypervolume
Tbar_occ <- rasterToPoints(raster("./Output/Post_processing/Tbarbigera/Tbar_Dist.asc"))#Getting a second species for comparison
Tbar_Envt <- extract(envtPC, Tbar_occ[,1:2])
Tbar_Envt <- Tbar_Envt[complete.cases(Tbar_Envt),];
Tbar_Hyp <- hypervolume(Tbar_Envt, repsperpoint = 5, bandwidth = 1, quantile = .1, name = "Tibouchina barbigera"); #Calculates hypervolume
Tbar_Hyp

#Fourth hypervolume
Tmel_occ <- rasterToPoints(raster("./Output/Post_processing/Tmelastomoides/Tmel_Dist.asc"))#Getting a second species for comparison
Tmel_Envt <- extract(envtPC, Tmel_occ[,1:2])
Tmel_Envt <- Tmel_Envt[complete.cases(Tmel_Envt),];
Tmel_Hyp <- hypervolume(Tmel_Envt, repsperpoint = 5, bandwidth = 1, quantile = .1, name = "Tibouchina melastomoides"); #Calculates hypervolume
Tmel_Hyp

hypervolumes_indices <- data.frame(input1 = character(), input2 = character(), Sorensen_overlap = numeric())
hypervolumes_indices

#Combining hypervolumes into a single set for comparison
hv_set <- hypervolume_set(Taeg_Hyp, Tasp_Hyp, check_memory=F);
hv_set2 <- hypervolume_set(Taeg_Hyp, Tbar_Hyp, check_memory=F);
hv_set3 <- hypervolume_set(Taeg_Hyp, Tmel_Hyp, check_memory=F);
hv_set4 <- hypervolume_set(Tasp_Hyp, Tbar_Hyp, check_memory=F);
hv_set5 <- hypervolume_set(Tasp_Hyp, Tmel_Hyp, check_memory=F);
hv_set6 <- hypervolume_set(Tbar_Hyp, Tmel_Hyp, check_memory=F);

#Calculate n-dimensional niche overlap using Sorensen index (see package info for details);
SI <- hypervolume_sorensen_overlap(hv_set6);
hypervolumes_indices <- rbind(hypervolumes_indices, cbind("Tbar", "Tmel", SI))
hypervolumes_indices
write.csv(hypervolumes_indices, "./Output/hypervolumes_indices.csv")
# Plot of overlap among hypervolumes in e-space
png("./Output/Taeg_Tasp_hvset.png")
plot(hv_set);
dev.off()

# Barplot comparison of species
barplot(get_volume(hv_set));

#To get the actual values used to create the bar plot
get_volume(hv_set);
#Note: the hypervolumes are named respective to the order they were entered into hypervolume_set(Line 105 above)
  ##So HV1 is gadMoHyp (Hypervolume of Gadus morhua), HV2 is arcGlHyp (Hypervolume of Arctogadus glacialis)

#OPTIONAL INTRODUCTION TO BIOMOD####
##########################################################
#Running BIOMOD: Preparing data
#========================================================

# Reshuffling data for BIOMOD
sp.data.GadMo <- codPoints[,2:3] #extract coordinates for presences
sp.data.GadMo$GadusMorhua <- 1 #create new variable with value 1
myRespName <- 'GadusMorhua' #assign a species name to the object
myResp<- as.numeric(sp.data.GadMo[,myRespName]) #Assigns a 1 to every occurrence point
latlong<-sp.data.GadMo[,1:2] #create lat long file
head(sp.data.GadMo)
 
#Format data & select pseudo-absence parameters

bio.data <- BIOMOD_FormatingData(resp.var = myResp, 
                                 #Species occurrences (presences, 1,  and/or absences, 0)
                                 expl.var = envtStack, 
                                 #Environmental raster data
                                 resp.xy = latlong, 
                                 #xy coordinates correlating with response variables
                                 resp.name = myRespName, 
                                 #The species name
                                 PA.nb.rep = 1, 
                                 #Specifies you want one set of pseudo-absences
                                 PA.nb.absences = 1000, 
                                 #Select any number of pseudo-absences#
                                 PA.strategy = 'random') 
                                #Selection strategy#

#Create list of BIOMOD algorithms

#mod.list<-c("GLM","MARS","FDA","GBM","RF","MAXENT.Phillips", "MAXENT.Tsuruoka" "GAM","CTA","ANN","SRE")
##all algorithms in biomod2
mod.list<-c("GLM","RF","GBM") ##Subset of models to use
mod.num<-length(mod.list) ##number of selected models
```

#Define settings for each algorithm
#========================================================
  
#Generalized linear model
GLMparams <- list(type = 'quadratic',
                  interaction.level = 0,
                  myFormula = NULL,
                  test = 'AIC',
                  family = 'binomial',
                  mustart = 0.5,
                  control = glm.control(epsilon = 1e-08, maxit = 50, trace = FALSE))

#Random forests
RFparams <- list( do.classif = TRUE,
                  ntree = 500,
                  mtry = 'default')       

#  Boosted regression trees
GBMparams <- list(distribution = 'bernoulli',
                  n.trees = 2000,
                  cv.folds = 1)      

#Load modeling options
bio.option <- BIOMOD_ModelingOptions(
  GLM = GLMparams,
  GBM = GBMparams,
  GAM = NULL, CTA = NULL,
  ANN = NULL, SRE = NULL,
  FDA = NULL, MARS = NULL,
  RF = RFparams, MAXENT.Phillips = NULL, 
  MAXENT.Tsuruoka = NULL)

#Generating niche models
#========================================================
setwd("~/Dropbox/ENMSeminar/Labs:Homeworks/Lab9/Data/")
bio.output.cv <- BIOMOD_Modeling(data=bio.data, 
                                 models = mod.list, models.options = bio.option,
                                 NbRunEval=1, #number of randomizations to run#
                                 DataSplit=75, #percentage of data to use for model training#
                                 Yweights=NULL, #Index of detectability
                                 Prevalence=NULL, #For building weighted models
                                 VarImport=1, #Number of permutations for variable importance
                                 models.eval.meth = c("KAPPA","TSS","ROC"), SaveObj = TRUE, 
                                 rescal.all.models = FALSE, do.full.models=FALSE)


#Get BIOMOD model evaluations
#========================================================

mets <- get_evaluations(bio.output.cv)
mets

# Visualizing BIOMOD ENM predictions
#========================================================

#Select mode files from memory
mods.choose<-c(
grep("_Full_GLM",get_built_models(bio.output.cv),value=T),
grep("_Full_RF",get_built_models(bio.output.cv),value=T),
grep("Full_GBM",get_built_models(bio.output.cv),value=T)) #Select algorithms to include#

#Project ENM from e-space to g-space
bio.proj1<-BIOMOD_Projection(modeling.output=bio.output.cv,
                new.env=envtStack,
                proj.name="contemp",
                xy.new.env = NULL,
                selected.models = "all",
                binary.meth = 'TSS',
                filtered.meth = NULL,
                compress = "xz",
                build.clamping.mask = FALSE,
                do.stack=TRUE)
#View results
plot(bio.proj1) #View all prediction maps#
plot(bio.proj1, str.grep="RF") #View specific prediction map#

#Save layers as rasters
list.files("GadusMorhua/proj_contemp/")
contemp.proj<-stack("GadusMorhua/proj_contemp/proj_contemp_GadusMorhua.grd")#Probability maps#
map.GLM<-raster(contemp.proj,layer=1) #GLM map#
map.RF<-raster(contemp.proj,layer=2) #RF Map#
map.GBM<-raster(contemp.proj,layer=3) #GBM Map#

#Show model results
plot(map.GLM)
plot(map.RF)
plot(map.GBM)

#Load binary maps
list.files("GadusMorhua/proj_contemp/")
contemp.proj.bin<-stack("GadusMorhua/proj_contemp/proj_contemp_GadusMorhua_TSSbin.grd") #Binary maps#
map.bin.GLM<-raster(contemp.proj.bin,layer=1) #GLM map#
map.bin.RF<-raster(contemp.proj.bin,layer=2) #RF Map#
map.bin.GBM<-raster(contemp.proj.bin,layer=3) #GBM Map#
 
#View binary maps
plot(map.bin.GLM)
plot(map.bin.RF)
plot(map.bin.GBM)