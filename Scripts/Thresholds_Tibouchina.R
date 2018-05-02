library(raster);

#First you will get the necessary data. You will need the occurrence point .csv file you used to train the model and an ascii file produced by Maxent (it will have the same name as the .png of your model results you submitted for lab last week).

Taegopogon_Raster <- raster("./Lab5/Taegopogon/Run3_projectedNW_noalt/Tibouchina_aegopogon_median.asc"); #This is an ascii produced by Maxent with your model results
Taegopogon_Points <- read.csv("./Lab4/clean_Taeg_PointsSPOCC.csv"); #These are the points you used to train the model

#First you will calculate the thresholds you will use. In this case, you'll find thresholds based on score quantiles from your results raster.
threshold <- quantile(Taegopogon_Raster);
threshold
#Generates plots of thresholds and statistics to illustrate sensitivity and specificity tradeoffs.
proportionPresent <- vector(mode = "list", length(threshold))
truePresences <- vector(mode = "list", length(threshold))
count <- 1;
while (count <= length(threshold)){
  m <- c(0, threshold[count], 0,  threshold[count], 1, 1); #This vector tells R that from 0 to the threshold, reclassify the raster cells as 0s, and from the threshold to 1, reclassify the cells as 1s.
  rclmat <- matrix(m, ncol=3, byrow=TRUE); #This turns the vector into a matrix
  threshed <- reclassify(Taegopogon_Raster, rcl = rclmat); #This reclassifies your raster.
  plot(threshed, main = paste("Threshold: ", threshold[count], sep = "")); #Plot the resulting raster, with the occurrence points.
  points(Taegopogon_Points[,2:3], pch = ".") #Plots your occurrence points
  proportionPresent[[count]] <- table(values(threshed))[2] / (table(values(threshed))[1] + table(values(threshed))[2]) #Calculates the proportion of the training region over which presences are predicted
  truePresences[[count]] <- sum(na.omit(extract(threshed, Taegopogon_Points[,2:3]))) #Counts the number of true presences predicted by the model
  count <- count + 1;
}

#Making and saving the threshold table
thresholdTable <- cbind(threshold, truePresences, proportionPresent) #Puts the results of your loop into a table
write.csv(thresholdTable, "./Lab7/ThresholdTable.csv", row.names = F) #Writes the results into a table
