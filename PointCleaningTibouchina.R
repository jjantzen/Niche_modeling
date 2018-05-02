library(spocc); #For getting georefernces
library(raster); #For loading and manipulating rasters
library(scrubr); #Package introduced for data cleaning
library(spatstat); #Spatial statistics package with method for calculating nearest neighbor distance

#Download occurrences from spocc
spocc_Tmelastomoides<- occ(query = "Tibouchina melastomoides", from = c('idigbio', 'gbif'), has_coords = T)
spocc_Taegopogon <- occ(query = "Tibouchina aegopogon", from = c('idigbio', 'gbif'), has_coords = T)
spocc_Taspera <- occ(query = "Tibouchina aspera", from = c('idigbio', 'gbif'), has_coords = T)
spocc_Tbarbigera <- occ(query = "Tibouchina barbigera", from = c('idigbio', 'gbif'), has_coords = T)

#fix names to query
spocc_Tmelastomoides <- fixnames(spocc_Tmelastomoides, how = "query")
spocc_Taegopogon <- fixnames(spocc_Taegopogon, how = "query")
spocc_Taspera <- fixnames(spocc_Taspera, how = "query")
spocc_Tbarbigera <- fixnames(spocc_Tbarbigera, how = "query")

#you will note the structure of the object for spocc is much different than that of rgbif, with separate slots for each of the databases that were queried, and not all the databases may return results.
spocc_Tmelastomoides_DF <- occ2df(spocc_Tmelastomoides)
spocc_Taegopogon_DF <- occ2df(spocc_Taegopogon)
spocc_Taspera_DF <- occ2df(spocc_Taspera)
spocc_Tbarbigera_DF <- occ2df(spocc_Tbarbigera)

#standardize dates
spocc_Tmelastomoides_DF <- date_standardize(spocc_Tmelastomoides_DF, "%d%b%Y")
spocc_Taegopogon_DF <- date_standardize(spocc_Taegopogon_DF, "%d%b%Y")
spocc_Taspera_DF <- date_standardize(spocc_Taspera_DF, "%d%b%Y")
spocc_Tbarbigera_DF <- date_standardize(spocc_Tbarbigera_DF, "%d%b%Y")

#Note number of raw points
paste("Raw points from spocc T_melastomoides query: ", nrow(spocc_Tmelastomoides_DF), " points.", sep = "")
paste("Raw points from spocc T_aegopogon query: ", nrow(spocc_Taegopogon_DF), " points.", sep = "")
paste("Raw points from spocc T_aspera query: ", nrow(spocc_Taspera_DF), " points.", sep = "")
paste("Raw points from spocc T_barbigera query: ", nrow(spocc_Tbarbigera_DF), " points.", sep = "")

#Step 1: remove incomplete records and unlikely coordinates
scrubbed_Tmelastomoides <- coord_incomplete(spocc_Tmelastomoides_DF)
scrubbed_Taegopogon <- coord_incomplete(spocc_Taegopogon_DF)
scrubbed_Taspera <- coord_incomplete(spocc_Taspera_DF)
scrubbed_Tbarbigera <- coord_incomplete(spocc_Tbarbigera_DF)

scrubbed_Tmelastomoides <- coord_unlikely(scrubbed_Tmelastomoides)
scrubbed_Taegopogon <- coord_unlikely(scrubbed_Taegopogon)
scrubbed_Taspera <- coord_unlikely(scrubbed_Taspera)
scrubbed_Tbarbigera <- coord_unlikely(scrubbed_Tbarbigera)

#Note number of rows post scrubr
paste("Step 1: Post scrubr T_melastomoides: ", nrow(scrubbed_Tmelastomoides), " points left.", sep = "")
paste("Step 1: Post scrubr T_aegopogon: ", nrow(scrubbed_Taegopogon), " points left.", sep = "")
paste("Step 1: Post scrubr T_aspera: ", nrow(scrubbed_Taspera), " points left.", sep = "")
paste("Step 1: Post scrubr T_barbigera: ", nrow(scrubbed_Tbarbigera), " points left.", sep = "")

#Step 2: Removing duplicate dates and localities
scrubbed_Tmelastomoides <- scrubbed_Tmelastomoides[,-6]; #Removes unique, database-specific keys
unique_Tmelastomoides <- unique(scrubbed_Tmelastomoides[,-4]); #Removes duplicate points (not considering data provider)
scrubbed_Taegopogon <- scrubbed_Taegopogon[,-6]; #Removes unique, database-specific keys
unique_Taegopogon <- unique(scrubbed_Taegopogon[,-4]); #Removes duplicate points (not considering data provider)
scrubbed_Taspera <- scrubbed_Taspera[,-6]; #Removes unique, database-specific keys
unique_Taspera <- unique(scrubbed_Taspera[,-4]); #Removes duplicate points (not considering data provider)
scrubbed_Tbarbigera <- scrubbed_Tbarbigera[,-6]; #Removes unique, database-specific keys
unique_Tbarbigera <- unique(scrubbed_Tbarbigera[,-4]); #Removes duplicate points (not considering data provider)

#Note number of entries after step 2
paste("Step 2: Removing duplicate dates and localities T_melastomoides: ", nrow(unique_Tmelastomoides), " points left.", sep = "")
paste("Step 2: Removing duplicate dates and localities T_aegopogon: ", nrow(unique_Taegopogon), " points left.", sep = "")
paste("Step 2: Removing duplicate dates and localities T_aspera: ", nrow(unique_Taspera), " points left.", sep = "")
paste("Step 2: Removing duplicate dates and localities T_barbigera: ", nrow(unique_Tbarbigera), " points left.", sep = "")

#Step 3: Removing points with no environmental data
terrestrialAltitude <- raster("./Lab4/terrestrialAltitude.asc")
south_america <- readOGR("./Lab4/joined/joined.shp")
#crop terrestrialAltitude to south america
crop_terrAlt <- crop(terrestrialAltitude, extent(south_america))
extent(south_america)
Tmel_Extract <- extract(crop_terrAlt, sapply(unique_Tmelastomoides[2:3], as.numeric))
Taeg_Extract <- extract(crop_terrAlt, sapply(unique_Taegopogon[2:3], as.numeric))
Tasp_Extract <- extract(crop_terrAlt, sapply(unique_Taspera[2:3], as.numeric))
Tbar_Extract <- extract(crop_terrAlt, sapply(unique_Tbarbigera[2:3], as.numeric))

Tmel_Extract<- cbind(unique_Tmelastomoides, Tmel_Extract)
Taeg_Extract<- cbind(unique_Taegopogon, Taeg_Extract)
Tasp_Extract<- cbind(unique_Taspera, Tasp_Extract)
Tbar_Extract<- cbind(unique_Tbarbigera, Tbar_Extract)

clean_Tmel_Extract <- Tmel_Extract[complete.cases(Tmel_Extract[,5]),]
clean_Taeg_Extract <- Taeg_Extract[complete.cases(Taeg_Extract[,5]),]
clean_Tasp_Extract <- Tasp_Extract[complete.cases(Tasp_Extract[,5]),]
clean_Tbar_Extract <- Tbar_Extract[complete.cases(Tbar_Extract[,5]),]

#Number of rows after step 3
paste("Step 3: Remove points with no environmental data T_melastomoides: ", nrow(clean_Tmel_Extract), " points left.", sep = "")
paste("Step 3: Remove points with no environmental data T_aegopogon: ", nrow(clean_Taeg_Extract), " points left.", sep = "")
paste("Step 3: Remove points with no environmental data T_aspera: ", nrow(clean_Tasp_Extract), " points left.", sep = "")
paste("Step 3: Remove points with no environmental data T_barbigera: ", nrow(clean_Tbar_Extract), " points left.", sep = "")

#Step 4: Reduce points to resolution of environmental data
rasterResolution <- max(res(terrestrialAltitude))

while(min(nndist(clean_Tmel_Extract[,2:3])) < rasterResolution){
  nnD <- nndist(clean_Tmel_Extract[,2:3])
  clean_Tmel_Extract <- clean_Tmel_Extract[-(which(min(nnD) == nnD)[1]),]
}
row.names(clean_Tmel_Extract) <- seq(nrow(clean_Tmel_Extract))

while(min(nndist(clean_Taeg_Extract[,2:3])) < rasterResolution){
  nnD <- nndist(clean_Taeg_Extract[,2:3])
  clean_Taeg_Extract <- clean_Taeg_Extract[-(which(min(nnD) == nnD)[1]),]
}
row.names(clean_Taeg_Extract) <- seq(nrow(clean_Taeg_Extract))

while(min(nndist(clean_Tasp_Extract[,2:3])) < rasterResolution){
  nnD <- nndist(clean_Tasp_Extract[,2:3])
  clean_Tasp_Extract <- clean_Tasp_Extract[-(which(min(nnD) == nnD)[1]),]
}
row.names(clean_Tasp_Extract) <- seq(nrow(clean_Tasp_Extract))

while(min(nndist(clean_Tbar_Extract[,2:3])) < rasterResolution){
  nnD <- nndist(clean_Tbar_Extract[,2:3])
  clean_Tbar_Extract <- clean_Tbar_Extract[-(which(min(nnD) == nnD)[1]),]
}
row.names(clean_Tbar_Extract) <- seq(nrow(clean_Tbar_Extract))

#Note number of rows after step 4
paste("Step 4: Resolution reduction T_melastomoides: ", nrow(clean_Tmel_Extract), " points left.", sep = "")
paste("Step 4: Resolution reduction T_aegopogon: ", nrow(clean_Taeg_Extract), " points left.", sep = "")
paste("Step 4: Resolution reduction T_aspera: ", nrow(clean_Tasp_Extract), " points left.", sep = "")
paste("Step 4: Resolution reduction T_barbigera: ", nrow(clean_Tbar_Extract), " points left.", sep = "")


#Plotting the results to see how things shook out.
plot(crop_terrAlt, main="Altitude", col = gray.colors(10, start = 0.3, end = 0.9, gamma = 2.2, alpha = NULL))
#plot all points
points(spocc_Tmelastomoides_DF[2:3], pch = 16, cex = 1)
#plot only cleanest dataset
points(clean_Tmel_Extract[,2:3], pch = 16, cex = 1, col = "red")
legend(-175, 90, c("Raw T melastomoides", "Clean T melastomoides"), pch = c(16,16), col=c("black", "red"))

plot(terrestrialAltitude, main="Altitude", col = gray.colors(10, start = 0.3, end = 0.9, gamma = 2.2, alpha = NULL))
#plot all points
points(spocc_Taegopogon_DF[2:3], pch = 16, cex = 1)
#plot only cleanest dataset
points(clean_Taeg_Extract[,2:3], pch = 16, cex = 1, col = "red")
legend(-175, 90, c("Raw T aegopogon", "Clean T aegopogon"), pch = c(16,16), col=c("black", "red"))

plot(terrestrialAltitude, main="Altitude", col = gray.colors(10, start = 0.3, end = 0.9, gamma = 2.2, alpha = NULL))
#plot all points
points(spocc_Taspera_DF[2:3], pch = 16, cex = 1)
#plot only cleanest dataset
points(clean_Tasp_Extract[,2:3], pch = 16, cex = 1, col = "red")
legend(-175, 90, c("Raw T aspera", "Clean T aspera"), pch = c(16,16), col=c("black", "red"))

plot(terrestrialAltitude, main="Altitude", col = gray.colors(10, start = 0.3, end = 0.9, gamma = 2.2, alpha = NULL))
#plot all points
points(spocc_Tbarbigera_DF[2:3], pch = 16, cex = 1)
#plot only cleanest dataset
points(clean_Tbar_Extract[,2:3], pch = 16, cex = 1, col = "red")
legend(-175, 90, c("Raw T barbigera", "Clean T barbigera"), pch = c(16,16), col=c("black", "red"))

#Saving your clean data
write.csv(clean_Tmel_Extract[,1:4], "./Lab4/clean_Tmel_PointsSPOCC.csv", row.names = F)
write.csv(clean_Taeg_Extract[,1:4], "./Lab4/clean_Taeg_PointsSPOCC.csv", row.names = F)
write.csv(clean_Tasp_Extract[,1:4], "./Lab4/clean_Tasp_PointsSPOCC.csv", row.names = F)
write.csv(clean_Tbar_Extract[,1:4], "./Lab4/clean_Tbar_PointsSPOCC.csv", row.names = F)
