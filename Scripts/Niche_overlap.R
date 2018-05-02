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

install.packages("devtools")
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
Taeg_model <- raster("./Output/Taegopogon/Basic7_soil/Tibouchina_aegopogon_NW_median.asc");
suitabilityScores <- extract(Taeg_model, Taeg_Points[,2:3]); #Extract suitability scores at occurrences
suitabilityScores <- suitabilityScores[complete.cases(suitabilityScores)] #Clean out no data values
threshold <- sort(suitabilityScores, decreasing = T)[round(length(suitabilityScores)*.95,0)] #Takes 5th percentile suitability score
m <- c(0, threshold, 0,  threshold, 1, 1); #Create a list of thresholds in the following format: (Minimum, maximum, valueToChange)
rclmat <- matrix(m, ncol=3, byrow=TRUE); #Changes the threshold list to a matrix
Taeg_Dist <- reclassify(Taeg_model, rcl = rclmat); #Reclassify suitability surface to presence/absence
plot(Taeg_Dist)
#NOTE: codDist can be saved like any other raster file (refer to Lab 2);
writeRaster(Taeg_Dist, "./Output/Post_processing/Identical_models/Taeg_Dist_95", format = "ascii", overwrite = TRUE);

#Second species
Tasp_model <- raster("./Output/Taspera/Basic7_soil/Tibouchina_aspera_NW_median.asc");
suitabilityScores <- extract(Tasp_model, Tasp_Points[,2:3]); #Extract suitability scores at occurrences
suitabilityScores <- suitabilityScores[complete.cases(suitabilityScores)] #Clean out no data values
threshold <- sort(suitabilityScores, decreasing = T)[round(length(suitabilityScores)*.95,0)] #Takes 5th percentile suitability score
m <- c(0, threshold, 0,  threshold, 1, 1); #Create a list of thresholds in the following format: (Minimum, maximum, valueToChange)
rclmat <- matrix(m, ncol=3, byrow=TRUE); #Changes the threshold list to a matrix
Tasp_Dist <- reclassify(Tasp_model, rcl = rclmat); #Reclassify suitability surface to presence/absence
plot(Tasp_Dist)
#NOTE: codDist can be saved like any other raster file (refer to Lab 2);
writeRaster(Tasp_Dist, "./Output/Post_processing/Identical_models/Tasp_Dist_95", format = "ascii", overwrite = TRUE);

#Second species
Tbar_model <- raster("./Output/Tbarbigera/Basic7_soil/Tibouchina_barbigera_NW_median.asc");
suitabilityScores <- extract(Tbar_model, Tbar_Points[,2:3]); #Extract suitability scores at occurrences
suitabilityScores <- suitabilityScores[complete.cases(suitabilityScores)] #Clean out no data values
threshold <- sort(suitabilityScores, decreasing = T)[round(length(suitabilityScores)*.75,0)] #Takes 5th percentile suitability score
m <- c(0, threshold, 0,  threshold, 1, 1); #Create a list of thresholds in the following format: (Minimum, maximum, valueToChange)
rclmat <- matrix(m, ncol=3, byrow=TRUE); #Changes the threshold list to a matrix
Tbar_Dist <- reclassify(Tbar_model, rcl = rclmat); #Reclassify suitability surface to presence/absence
plot(Tbar_Dist)
#NOTE: codDist can be saved like any other raster file (refer to Lab 2);
writeRaster(Tbar_Dist, "./Output/Post_processing/Identical_models/Tbar_Dist_75", format = "ascii", overwrite = TRUE);

#Second species
Tmel_model <- raster("./Output/Tmelastomoides/Basic7_soil/Tibouchina_melastomoides_NW_median.asc");
suitabilityScores <- extract(Tmel_model, Tmel_Points[,2:3]); #Extract suitability scores at occurrences
suitabilityScores <- suitabilityScores[complete.cases(suitabilityScores)] #Clean out no data values
threshold <- sort(suitabilityScores, decreasing = T)[round(length(suitabilityScores)*.80,0)] #Takes 5th percentile suitability score
m <- c(0, threshold, 0,  threshold, 1, 1); #Create a list of thresholds in the following format: (Minimum, maximum, valueToChange)
rclmat <- matrix(m, ncol=3, byrow=TRUE); #Changes the threshold list to a matrix
Tmel_Dist <- reclassify(Tmel_model, rcl = rclmat); #Reclassify suitability surface to presence/absence
plot(Tmel_Dist)
#NOTE: codDist can be saved like any other raster file (refer to Lab 2);
writeRaster(Tmel_Dist, "./Output/Post_processing/Identical_models/Tmel_Dist_80", format = "ascii", overwrite = TRUE);

#Calculate predicted niche overlap
pno_envtList <- list.files(path = "./Output/Post_processing/Unreduced_environment/", pattern = ".asc", full.names=TRUE)
pno_envtStack<- stack(pno_envtList)

#Elevation
Taeg_elevPNO <- pno(path_bioclim = "./Output/Post_processing/Unreduced_environment/_Taegopogon_alt.asc", path_model = "./Output/Post_processing/For_pnos/Aeg/", bin_number = 50); 
Tasp_elevPNO <- pno(path_bioclim = "./Output/Post_processing/Unreduced_environment/_Taegopogon_alt.asc", path_model = "./Output/Post_processing/For_pnos/Asp/", bin_number = 50)
Tbar_elevPNO <- pno(path_bioclim = "./Output/Post_processing/Unreduced_environment/_Taegopogon_alt.asc", path_model = "./Output/Post_processing/For_pnos/Bar/", bin_number = 50)
Tmel_elevPNO <- pno(path_bioclim = "./Output/Post_processing/Unreduced_environment/_Taegopogon_alt.asc", path_model = "./Output/Post_processing/For_pnos/Mel/", bin_number = 50)

Taeg_elevPNO_df <- as.data.frame(Taeg_elevPNO)
Tasp_elevPNO_df <- as.data.frame(Tasp_elevPNO, col.names = c("variable", "probability"))
Tbar_elevPNO_df <- as.data.frame(Tbar_elevPNO, col.names = c("variable", "probability"))
Tmel_elevPNO_df <- as.data.frame(Tmel_elevPNO, col.names = c("variable", "probability"))

Taeg_elevPNO_df <- cbind(Taeg_elevPNO_df, "Tibouchina aegopogon")
Tasp_elevPNO_df <- cbind(Tasp_elevPNO_df, "Tibouchina aspera")
Tbar_elevPNO_df <- cbind(Tbar_elevPNO_df, "Tibouchina barbigera")
Tmel_elevPNO_df <- cbind(Tmel_elevPNO_df, "Tibouchina melastomoides")

colnames(Taeg_elevPNO_df) <- c("variable", "probability", "species")
colnames(Tasp_elevPNO_df) <- c("variable", "probability", "species")
colnames(Tbar_elevPNO_df) <- c("variable", "probability", "species")
colnames(Tmel_elevPNO_df) <- c("variable", "probability", "species")

All_elev_pnos <- rbind(Taeg_elevPNO_df, Tasp_elevPNO_df, Tbar_elevPNO_df, Tmel_elevPNO_df)
png("./Output/Post_processing/Identical_models/PNO_elevation.png")
ggplot(All_elev_pnos)+
  geom_line(aes(x = variable, y = probability, colour = factor(species)))+
  ylab("Probability of predicted niche occupancy")+
  xlab("Elevation above sea level (m)")+
  labs(title = "Altitude")+
  theme(axis.title.y = element_text(size = rel(1.8)))+
  theme(axis.title.x = element_text(size = rel(1.8)))+
  theme(plot.title = element_text(size = rel(2)))
dev.off()        

#Precipitation
Taeg_ann_ppt_PNO <- pno(path_bioclim = "./Output/Post_processing/Unreduced_environment/_Taegopogon_bio12.asc", path_model = "./Output/Post_processing/For_pnos/Aeg/", bin_number = 50); 
Tasp_ann_ppt_PNO <- pno(path_bioclim = "./Output/Post_processing/Unreduced_environment/_Taegopogon_bio12.asc", path_model = "./Output/Post_processing/For_pnos/Asp/", bin_number = 50)
Tbar_ann_ppt_PNO <- pno(path_bioclim = "./Output/Post_processing/Unreduced_environment/_Taegopogon_bio12.asc", path_model = "./Output/Post_processing/For_pnos/Bar/", bin_number = 50)
Tmel_ann_ppt_PNO <- pno(path_bioclim = "./Output/Post_processing/Unreduced_environment/_Taegopogon_bio12.asc", path_model = "./Output/Post_processing/For_pnos/Mel/", bin_number = 50)

Taeg_ann_ppt_PNO_df <- as.data.frame(Taeg_ann_ppt_PNO)
Tasp_ann_ppt_PNO_df <- as.data.frame(Tasp_ann_ppt_PNO)
Tbar_ann_ppt_PNO_df <- as.data.frame(Tbar_ann_ppt_PNO)
Tmel_ann_ppt_PNO_df <- as.data.frame(Tmel_ann_ppt_PNO)

Taeg_ann_ppt_PNO_df <- cbind(Taeg_ann_ppt_PNO_df, "Tibouchina aegopogon")
Tasp_ann_ppt_PNO_df <- cbind(Tasp_ann_ppt_PNO_df, "Tibouchina aspera")
Tbar_ann_ppt_PNO_df <- cbind(Tbar_ann_ppt_PNO_df, "Tibouchina barbigera")
Tmel_ann_ppt_PNO_df <- cbind(Tmel_ann_ppt_PNO_df, "Tibouchina melastomoides")

colnames(Taeg_ann_ppt_PNO_df) <- c("variable", "probability", "species")
colnames(Tasp_ann_ppt_PNO_df) <- c("variable", "probability", "species")
colnames(Tbar_ann_ppt_PNO_df) <- c("variable", "probability", "species")
colnames(Tmel_ann_ppt_PNO_df) <- c("variable", "probability", "species")

All_ann_ppt__pnos <- rbind(Taeg_ann_ppt_PNO_df, Tasp_ann_ppt_PNO_df, Tbar_ann_ppt_PNO_df, Tmel_ann_ppt_PNO_df)
png("./Output/Post_processing/Identical_models/PNO_ann_ppt.png")
ggplot(All_ann_ppt__pnos)+
  geom_line(aes(x = variable, y = probability, colour = factor(species)))+
  ylab("Probability?")+
  xlab("Annual Precipitation (mm)")+
  labs(title = "Predicted niche occupancy for annual precipitation (mm)", colour = "Species")+
  xlim(0, 5000)
dev.off()
#Precipitation of the driest month
Taeg_ppt_dryPNO <- pno(path_bioclim = "./Output/Post_processing/Unreduced_environment/_Taegopogon_bio14.asc", path_model = "./Output/Post_processing/For_pnos/Aeg/", bin_number = 50); 
Tasp_ppt_dryPNO <- pno(path_bioclim = "./Output/Post_processing/Unreduced_environment/_Taegopogon_bio14.asc", path_model = "./Output/Post_processing/For_pnos/Asp/", bin_number = 50)
Tbar_ppt_dryPNO <- pno(path_bioclim = "./Output/Post_processing/Unreduced_environment/_Taegopogon_bio14.asc", path_model = "./Output/Post_processing/For_pnos/Bar/", bin_number = 50)
Tmel_ppt_dryPNO <- pno(path_bioclim = "./Output/Post_processing/Unreduced_environment/_Taegopogon_bio14.asc", path_model = "./Output/Post_processing/For_pnos/Mel/", bin_number = 50)

Taeg_ppt_dryPNO_df <- as.data.frame(Taeg_ppt_dryPNO)
Tasp_ppt_dryPNO_df <- as.data.frame(Tasp_ppt_dryPNO)
Tbar_ppt_dryPNO_df <- as.data.frame(Tbar_ppt_dryPNO)
Tmel_ppt_dryPNO_df <- as.data.frame(Tmel_ppt_dryPNO)

Taeg_ppt_dryPNO_df <- cbind(Taeg_ppt_dryPNO_df, "Tibouchina aegopogon")
Tasp_ppt_dryPNO_df <- cbind(Tasp_ppt_dryPNO_df, "Tibouchina aspera")
Tbar_ppt_dryPNO_df <- cbind(Tbar_ppt_dryPNO_df, "Tibouchina barbigera")
Tmel_ppt_dryPNO_df <- cbind(Tmel_ppt_dryPNO_df, "Tibouchina melastomoides")

colnames(Taeg_ppt_dryPNO_df) <- c("variable", "probability", "species")
colnames(Tasp_ppt_dryPNO_df) <- c("variable", "probability", "species")
colnames(Tbar_ppt_dryPNO_df) <- c("variable", "probability", "species")
colnames(Tmel_ppt_dryPNO_df) <- c("variable", "probability", "species")

All_ppt_drypnos <- rbind(Taeg_ppt_dryPNO_df, Tasp_ppt_dryPNO_df, Tbar_ppt_dryPNO_df, Tmel_ppt_dryPNO_df)

ggplot(All_ppt_drypnos)+
  geom_line(aes(x = variable, y = probability, colour = factor(species)))+
  ylab("Probability?")+
  xlab("Precipitation of driest month (mm)")+
  labs(title = "Predicted niche occupancy for precipitation of driest month (mm) for four species of Tibouchina", colour = "Species")+
  xlim(0, 200)

#Mean diurnal range
Taeg_mdr_PNO <- pno(path_bioclim = "./Output/Post_processing/Unreduced_environment/_Taegopogon_bio2.asc", path_model = "./Output/Post_processing/For_pnos/Aeg/", bin_number = 50); 
Tasp_mdr_PNO <- pno(path_bioclim = "./Output/Post_processing/Unreduced_environment/_Taegopogon_bio2.asc", path_model = "./Output/Post_processing/For_pnos/Asp/", bin_number = 50)
Tbar_mdr_PNO <- pno(path_bioclim = "./Output/Post_processing/Unreduced_environment/_Taegopogon_bio2.asc", path_model = "./Output/Post_processing/For_pnos/Bar/", bin_number = 50)
Tmel_mdr_PNO <- pno(path_bioclim = "./Output/Post_processing/Unreduced_environment/_Taegopogon_bio2.asc", path_model = "./Output/Post_processing/For_pnos/Mel/", bin_number = 50)

Taeg_mdr_PNO_df <- as.data.frame(Taeg_mdr_PNO)
Tasp_mdr_PNO_df <- as.data.frame(Tasp_mdr_PNO)
Tbar_mdr_PNO_df <- as.data.frame(Tbar_mdr_PNO)
Tmel_mdr_PNO_df <- as.data.frame(Tmel_mdr_PNO)

Taeg_mdr_PNO_df <- cbind(Taeg_mdr_PNO_df, "Tibouchina aegopogon")
Tasp_mdr_PNO_df <- cbind(Tasp_mdr_PNO_df, "Tibouchina aspera")
Tbar_mdr_PNO_df <- cbind(Tbar_mdr_PNO_df, "Tibouchina barbigera")
Tmel_mdr_PNO_df <- cbind(Tmel_mdr_PNO_df, "Tibouchina melastomoides")

colnames(Taeg_mdr_PNO_df) <- c("variable", "probability", "species")
colnames(Tasp_mdr_PNO_df) <- c("variable", "probability", "species")
colnames(Tbar_mdr_PNO_df) <- c("variable", "probability", "species")
colnames(Tmel_mdr_PNO_df) <- c("variable", "probability", "species")

All_mdr_pnos <- rbind(Taeg_mdr_PNO_df, Tasp_mdr_PNO_df, Tbar_mdr_PNO_df, Tmel_mdr_PNO_df)
png("./Output/Post_processing/Identical_models/PNO_MDR.png")
ggplot(All_mdr_pnos)+
  geom_line(aes(x = variable, y = probability, colour = factor(species)))+
  ylab("Probability of predicted niche occupancy")+
  xlab("Mean Diurnal Range")+
  labs(title = "Mean Diurnal Range")+
  theme(axis.title.y = element_text(size = rel(1.8)))+
  theme(axis.title.x = element_text(size = rel(1.8)))+
  theme(plot.title = element_text(size = rel(2)))
dev.off()
