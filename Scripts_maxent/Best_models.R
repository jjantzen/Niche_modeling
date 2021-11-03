#For best models, not identical models
#Calculate niche overlap in geographic space for the four species
#Pairwise comparisons?

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

#install.packages("devtools")
library(devtools)
install_github("danlwarren/ENMTools")
library(ENMTools)

#Get environmental data used to train your model

Taeg_envtList <- list.files(path = "./Layers/Taegopogon/Trimmed_Taegopogon", pattern = ".asc", full.names=TRUE)
Taeg_envtStack <- stack(Taeg_envtList);

Tasp_envtList <- list.files(path = "./Layers/Taspera/Small_Taspera", pattern = ".asc", full.names = TRUE)
Tasp_envtStack <- stack(Tasp_envtList)

Tbar_envtList <- list.files(path = "./Layers/Tbarbigera/Trimmed_Tbarbigera", pattern = ".asc", full.names = TRUE)
Tbar_envtStack <- stack(Tbar_envtList)

Tmel_envtList <- list.files(path = "./Layers/Tmelastomoides/Trimmed_Tmelastomoides", pattern = ".asc", full.names = TRUE)
Tmel_envtStack <- stack(Tmel_envtList)

#Get occurrence points
Taeg_Points <- read.csv("./Data/clean_Taeg_PointsSPOCC.csv")
Tasp_Points <- read.csv("./Data/clean_Tasp_PointsSPOCC_no_outlier.csv");
Tbar_Points <- read.csv("./Data/clean_Tbar_PointsSPOCC.csv")
Tmel_Points <- read.csv("./Data/clean_Tmel_PointsSPOCC.csv")

# Example thresholding to 95% suitabilty score. You can use any number, though.
Taeg_model <- raster("./Output/Taegopogon/Reduced_all_uncorr_3p/Tibouchina_aegopogon_NW_median.asc");
suitabilityScores <- extract(Taeg_model, Taeg_Points[,2:3]); #Extract suitability scores at occurrences
suitabilityScores <- suitabilityScores[complete.cases(suitabilityScores)] #Clean out no data values
threshold <- sort(suitabilityScores, decreasing = T)[round(length(suitabilityScores)*.90,0)] #Takes 5th percentile suitability score
m <- c(0, threshold, 0,  threshold, 1, 1); #Create a list of thresholds in the following format: (Minimum, maximum, valueToChange)
rclmat <- matrix(m, ncol=3, byrow=TRUE); #Changes the threshold list to a matrix
Taeg_Dist <- reclassify(Taeg_model, rcl = rclmat); #Reclassify suitability surface to presence/absence
plot(Taeg_Dist)
#NOTE: codDist can be saved like any other raster file (refer to Lab 2);
writeRaster(Taeg_Dist, "./Output/Post_processing/Best_models/Reduced_var/Taeg_Dist_90", format = "ascii", overwrite = TRUE);

#Second species
Tasp_model <- raster("./Output/Taspera/Reduced_all_uncorr_3p/Tibouchina_aspera_NW_median.asc");
suitabilityScores <- extract(Tasp_model, Tasp_Points[,2:3]); #Extract suitability scores at occurrences
suitabilityScores <- suitabilityScores[complete.cases(suitabilityScores)] #Clean out no data values
threshold <- sort(suitabilityScores, decreasing = T)[round(length(suitabilityScores)*.80,0)] #Takes 5th percentile suitability score
m <- c(0, threshold, 0,  threshold, 1, 1); #Create a list of thresholds in the following format: (Minimum, maximum, valueToChange)
rclmat <- matrix(m, ncol=3, byrow=TRUE); #Changes the threshold list to a matrix
Tasp_Dist <- reclassify(Tasp_model, rcl = rclmat); #Reclassify suitability surface to presence/absence
plot(Tasp_Dist)
#NOTE: codDist can be saved like any other raster file (refer to Lab 2);
writeRaster(Tasp_Dist, "./Output/Post_processing/Best_models/Reduced_var/Tasp_Dist_95", format = "ascii", overwrite = TRUE);

#Second species
Tbar_model <- raster("./Output/Tbarbigera/Reduced_all_uncorr/Tibouchina_barbigera_NW_median.asc");
suitabilityScores <- extract(Tbar_model, Tbar_Points[,2:3]); #Extract suitability scores at occurrences
suitabilityScores <- suitabilityScores[complete.cases(suitabilityScores)] #Clean out no data values
threshold <- sort(suitabilityScores, decreasing = T)[round(length(suitabilityScores)*.75,0)] #Takes 5th percentile suitability score
m <- c(0, threshold, 0,  threshold, 1, 1); #Create a list of thresholds in the following format: (Minimum, maximum, valueToChange)
rclmat <- matrix(m, ncol=3, byrow=TRUE); #Changes the threshold list to a matrix
Tbar_Dist <- reclassify(Tbar_model, rcl = rclmat); #Reclassify suitability surface to presence/absence
plot(Tbar_Dist)
#NOTE: codDist can be saved like any other raster file (refer to Lab 2);
writeRaster(Tbar_Dist, "./Output/Post_processing/Best_models/Reduced_var/Tbar_Dist_95", format = "ascii", overwrite = TRUE);

#Second species
Tmel_model <- raster("./Output/Tmelastomoides/Reduced_all_uncorr/Tibouchina_melastomoides_NW_median.asc");
suitabilityScores <- extract(Tmel_model, Tmel_Points[,2:3]); #Extract suitability scores at occurrences
suitabilityScores <- suitabilityScores[complete.cases(suitabilityScores)] #Clean out no data values
threshold <- sort(suitabilityScores, decreasing = T)[round(length(suitabilityScores)*.80,0)] #Takes 5th percentile suitability score
m <- c(0, threshold, 0,  threshold, 1, 1); #Create a list of thresholds in the following format: (Minimum, maximum, valueToChange)
rclmat <- matrix(m, ncol=3, byrow=TRUE); #Changes the threshold list to a matrix
Tmel_Dist <- reclassify(Tmel_model, rcl = rclmat); #Reclassify suitability surface to presence/absence
plot(Tmel_Dist)
#NOTE: codDist can be saved like any other raster file (refer to Lab 2);
writeRaster(Tmel_Dist, "./Output/Post_processing/Best_models/Reduced_Var/Tmel_Dist_80", format = "ascii", overwrite = TRUE)




#Calculating the extent of ENM-suitable habitat
cellAreas <- area(Taeg_Dist)*Taeg_Dist #Makes a raster of cell area
plot(cellAreas);
cellAreaMeasures <- rasterToPoints(cellAreas); #Adds up the area of each cell
Taeg_area <- sum(cellAreaMeasures[,3]) #prints an answer
Taeg_area

cellAreas <- area(Tasp_Dist)*Tasp_Dist #Makes a raster of cell area
plot(cellAreas);
cellAreaMeasures <- rasterToPoints(cellAreas); #Adds up the area of each cell
Tasp_area <- sum(cellAreaMeasures[,3]) #prints an answer
Tasp_area

cellAreas <- area(Tbar_Dist)*Tbar_Dist #Makes a raster of cell area
plot(cellAreas);
cellAreaMeasures <- rasterToPoints(cellAreas); #Adds up the area of each cell
Tbar_area <- sum(cellAreaMeasures[,3]) #prints an answer
Tbar_area

cellAreas <- area(Tmel_Dist)*Tmel_Dist #Makes a raster of cell area
plot(cellAreas);
cellAreaMeasures <- rasterToPoints(cellAreas); #Adds up the area of each cell
Tmel_area <- sum(cellAreaMeasures[,3]) #prints an answer
Tmel_area

#Calculate areas of overlap etc
areas_results <- data.frame(taxa = character(10), area = numeric(10), stringsAsFactors = FALSE)
areas_results[1,2] <- Taeg_area
areas_results$taxa[1] <- c("Tibouchina aegopogon")
areas_results[2,2] <- Tasp_area
areas_results$taxa[2] <- c("Tibouchina aspera")
areas_results[3,2] <- Tbar_area
areas_results$taxa[3] <- c("Tibouchina barbigera")
areas_results[4,2] <- Tmel_area
areas_results$taxa[4] <- c("Tibouchina melastomoides")



areas_results
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
