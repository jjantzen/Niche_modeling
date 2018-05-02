#Plotting model overlap in geographic space
library(dplyr)
library(raster)
library(ggplot2)
library(rgeos)

Taeg_model <- raster("./Output/Post_processing/Best_models/Reduced_var/Taeg_Dist_90.asc")
Tasp_model <- raster("./Output/Post_processing/Best_models/Reduced_var/Tasp_Dist_80.asc")
Tbar_model <- raster("./Output/Post_processing/Best_models/Reduced_var/Tbar_Dist_75.asc")
Tmel_model <- raster("./Output/Post_processing/Best_models/Reduced_var/Tmel_Dist_80.asc")
Tbar_model
Tmel_model
crs(Tmel_model) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
crs(Tbar_model) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
crs(Tasp_model) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
crs(Taeg_model) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"



colour1 <- makeTransparent("red")
backgroundcolour <- makeTransparent("lightgrey")
backgroundcolour <- makeTransparent(backgroundcolour)
colour2 <- makeTransparent("green")
colour3 <- makeTransparent("blue")
colour4 <- makeTransparent("yellow")

#Function to make colours transparent for plotting
makeTransparent<-function(someColor, alpha=100)
{
 newColor<-col2rgb(someColor)
 apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                             blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}

colour1
png("./Output/Post_processing/Plots_for_presentation/All_four_overlaid.png")
df1 <- as.data.frame(Taeg_model, xy = TRUE)
str(Taeg_model)
head(df1)
ggplot(df1)+
  geom_tile(aes(x = x, y = y, fill = ))
  ylab("Latitude")+
  xlab("Longitude")+
  labs(title = "Niche overlap in geographic space for four species of Tibouchina")

outline <- rasterToPolygons(Taeg_model, dissolve=TRUE)  
legend <- c("Tibouchina aegopogon", "Tibouchina aspera", "Tibouchina barbigera", "Tibouchina melastomoides")
image(Taeg_model, breaks = c(0, 0.5, 1), col = c(backgroundcolour, colour1), xlab = "Longitude", ylab = "Latitude")
#image(outline, lwd = 2, border= "red", add = TRUE)
image(Tasp_model, breaks = c(0, 0.5, 1), col = c(backgroundcolour, colour2), add= TRUE)
image(Tbar_model, breaks = c(0, 0.5, 1), col = c(backgroundcolour, colour3), add = TRUE)
image(Tmel_model, breaks = c(0, 0.5, 1), col = c(backgroundcolour, colour4), add = TRUE)
legend("bottomleft", legend = legend, pch = 20, pt.cex = 2, cex = 0.75, col = c(colour1, colour2, colour3, colour4))
dev.off()
legend()
#Calculate overlap 
Taeg_Tasp_overlap <- sum(Taeg_model, Tasp_model)
Taeg_Tbar_overlap <- sum(Taeg_model, Tbar_model)
Taeg_Tmel_overlap <- sum(Taeg_model, Tmel_model)
Tasp_Tbar_overlap <- sum(Tasp_model, Tbar_model)
Tasp_Tmel_overlap <- sum(Tasp_model, Tmel_model)
Tbar_Tmel_overlap <- sum(Tbar_model, Tmel_model)


#Making sure only values I'm expecting are included
values <- Taeg_Tbar_overlap@data@values %>% 
  unique()
values
#Get matrix of thresholds to reclassify raster data
matrix <- matrix(+c(0,1,1.1,2,0,1), nrow=2, ncol=3)
matrix
#Reclassify raster data to binary again (after summing) so 1 is overlap and everything else is 0
Taeg_Tasp_overlap_binary <- reclassify(Taeg_Tasp_overlap, rcl = matrix)
Taeg_Tbar_overlap_binary <- reclassify(Taeg_Tbar_overlap, rcl = matrix)
Taeg_Tmel_overlap_binary <- reclassify(Taeg_Tmel_overlap, rcl = matrix)
Tasp_Tbar_overlap_binary <- reclassify(Tasp_Tbar_overlap, rcl = matrix)
Tasp_Tmel_overlap_binary <- reclassify(Tasp_Tmel_overlap, rcl = matrix)
Tbar_Tmel_overlap_binary <- reclassify(Tbar_Tmel_overlap, rcl = matrix)

#Calculate areas of overlap
cellAreas <- area(Taeg_Tasp_overlap_binary)*Taeg_Tasp_overlap_binary #Makes a raster of cell area
plot(cellAreas);
cellAreaMeasures <- rasterToPoints(cellAreas); #Adds up the area of each cell
TaegTasp_area <- sum(cellAreaMeasures[,3])

cellAreas <- area(Taeg_Tbar_overlap_binary)*Taeg_Tbar_overlap_binary #Makes a raster of cell area
plot(cellAreas);
cellAreaMeasures <- rasterToPoints(cellAreas); #Adds up the area of each cell
TaegTbar_area <- sum(cellAreaMeasures[,3])
TaegTbar_area

cellAreas <- area(Taeg_Tmel_overlap_binary)*Taeg_Tmel_overlap_binary #Makes a raster of cell area
plot(cellAreas);
cellAreaMeasures <- rasterToPoints(cellAreas); #Adds up the area of each cell
TaegTmel_area <- sum(cellAreaMeasures[,3])
TaegTmel_area

cellAreas <- area(Tasp_Tbar_overlap_binary)*Tasp_Tbar_overlap_binary #Makes a raster of cell area
plot(cellAreas);
cellAreaMeasures <- rasterToPoints(cellAreas); #Adds up the area of each cell
TaspTbar_area <- sum(cellAreaMeasures[,3])
TaspTbar_area

cellAreas <- area(Tasp_Tmel_overlap_binary)*Tasp_Tmel_overlap_binary #Makes a raster of cell area
plot(cellAreas);
cellAreaMeasures <- rasterToPoints(cellAreas); #Adds up the area of each cell
TaspTmel_area <- sum(cellAreaMeasures[,3])
TaspTmel_area

cellAreas <- area(Tbar_Tmel_overlap_binary)*Tbar_Tmel_overlap_binary #Makes a raster of cell area
plot(cellAreas);
cellAreaMeasures <- rasterToPoints(cellAreas); #Adds up the area of each cell
TbarTmel_area <- sum(cellAreaMeasures[,3])
TbarTmel_area

areas_results$taxa[5] <- c("Tibouchina aegopogon x Tibouchina aspera")
areas_results$area[5] <- TaegTasp_area
areas_results$taxa[6] <- "Tibouchina aegopogon x Tibouchina barbigera"
areas_results$area[6] <- TaegTbar_area
areas_results$taxa[7] <- "Tibouchina aegopogon x Tibouchina melastomoides"
areas_results$area[7] <- TaegTmel_area
areas_results$taxa[8] <- "Tibouchina aspera x Tibouchina barbigera"
areas_results$area[8] <- TaspTbar_area
areas_results$taxa[9] <- "Tibouchina aspera x Tibouchina melastomoides"
areas_results$area[9] <- TaspTmel_area
areas_results$taxa[10] <- "Tibouchina barbigera x Tibouchina melastomoides"
areas_results$area[10] <- TbarTmel_area

areas_results
#Calculating for three and four overlapping areas 
TaegTaspTbar <- sum(Taeg_model, Tasp_model, Tbar_model)
TaegTaspTmel <- sum(Taeg_model, Tasp_model, Tmel_model)
TaspTbarTmel <- sum(Tasp_model, Tbar_model, Tmel_model)
TaegTbarTmel <- sum(Taeg_model, Tbar_model, Tmel_model)
Allfour <- sum(Taeg_model, Tasp_model, Tbar_model, Tmel_model)

values <- Allfour@data@values %>% 
  unique()
values
#Get matrix of thresholds to reclassify raster data
matrix <- matrix(c(0,2.1,2,3,0,1), nrow=2, ncol=3)
matrix
#Reclassify raster data to binary again (after summing) so 1 is overlap and everything else is 0
TaegTaspTbar_binary <- reclassify(TaegTaspTbar, rcl = matrix)
TaegTaspTmel_binary <- reclassify(TaegTaspTmel, rcl = matrix)
TaspTbarTmel_binary <- reclassify(TaspTbarTmel, rcl = matrix)
TaegTbarTmel_binary <- reclassify(TaegTbarTmel, rcl = matrix)

matrix <- matrix(c(0,3.1,3,4,0,1), nrow=2, ncol=3)
matrix
Allfour_binary <- reclassify(Allfour, rcl = matrix)
plot(Allfour_binary)

plot(TaegTbarTmel_binary)

plot(summed_models)
#Calculate areas of multiples overlapped and add to results table
cellAreas <- area(TaegTaspTbar_binary)*TaegTaspTbar_binary #Makes a raster of cell area
plot(cellAreas);
cellAreaMeasures <- rasterToPoints(cellAreas); #Adds up the area of each cell
TaegTaspTbar_area <- sum(cellAreaMeasures[,3])
TaegTaspTbar_area

cellAreas <- area(TaegTbarTmel_binary)*TaegTbarTmel_binary #Makes a raster of cell area
plot(cellAreas);
cellAreaMeasures <- rasterToPoints(cellAreas); #Adds up the area of each cell
TaegTbarTmel_area <- sum(cellAreaMeasures[,3])
TaegTbarTmel_area

cellAreas <- area(TaegTaspTmel_binary)*TaegTaspTmel_binary #Makes a raster of cell area
plot(cellAreas);
cellAreaMeasures <- rasterToPoints(cellAreas); #Adds up the area of each cell
TaegTaspTmel_area <- sum(cellAreaMeasures[,3])
TaegTaspTmel_area

cellAreas <- area(TaspTbarTmel_binary)*TaspTbarTmel_binary #Makes a raster of cell area
plot(cellAreas);
cellAreaMeasures <- rasterToPoints(cellAreas); #Adds up the area of each cell
TaspTbarTmel_area <- sum(cellAreaMeasures[,3])
TaspTbarTmel_area

cellAreas <- area(Allfour_binary)*Allfour_binary #Makes a raster of cell area
plot(cellAreas);
cellAreaMeasures <- rasterToPoints(cellAreas); #Adds up the area of each cell
Allfour_area <- sum(cellAreaMeasures[,3])
Allfour_area


areas_results <- rbind(areas_results, c("a",0))
areas_results



areas_results$taxa[11] <- c("Tibouchina aegopogon x Tibouchina aspera x Tibouchina barbigera")
areas_results$area[11] <- TaegTaspTbar_area

areas_results$taxa[12] <- "Tibouchina aegopogon x Tibouchina aspera x Tibouchina melastomoides"
areas_results$area[12] <- TaegTaspTmel_area

areas_results$taxa[13] <- "Tibouchina aegopogon x Tibouchina barbigera x Tibouchina melastomoides"
areas_results$area[13] <- TaegTbarTmel_area

areas_results$taxa[14] <- "Tibouchina aspera x Tibouchina barbigera x Tibouchina melastomoides"
areas_results$area[14] <- TaspTbarTmel_area

areas_results$taxa[15] <- "Tibouchina aegopogon x Tibouchina aspera x Tibouchina barbigera x Tibouchina melastomoides"
areas_results$area[15] <- Allfour_area
areas_results

write.csv(areas_results, "./Output/Post_processing/Best_models/Reduced_var/area_results.csv")
