#Alternative Script 1
#Cleaning data directly from download
#Modify this to database records
#Fix names to query
#Standardize dates
#Remove incomplete records and unlikely coordinates
#Remove duplicate dates and localities
#Remove points with no environmental data
#Reduce point resolution to match environmental data
#Plot to look for outliers


#Load libraries
library(spocc); #For getting georefernces
library(raster); #For loading and manipulating rasters
library(scrubr); #Package introduced for data cleaning
library(spatstat); #Spatial statistics package with method for calculating nearest neighbor distance

#Download occurrences from spocc
spocc_Tmelastomoides<- occ(query = "Tibouchina melastomoides", from = c('idigbio', 'gbif'), has_coords = T)

#Fix names to query term
spocc_Tmelastomoides <- fixnames(spocc_Tmelastomoides, how = "query")

#Make data as dataframe
spocc_Tmelastomoides_DF <- occ2df(spocc_Tmelastomoides)

#Standardize dates
spocc_Tmelastomoides_DF <- date_standardize(spocc_Tmelastomoides_DF, "%d%b%Y")

#Remove incomplete records and unlikely coordinates
scrubbed_Tmelastomoides <- coord_incomplete(spocc_Tmelastomoides_DF)

scrubbed_Tmelastomoides <- coord_unlikely(scrubbed_Tmelastomoides)

#Remove duplicate dates and localities
scrubbed_Tmelastomoides <- scrubbed_Tmelastomoides[,-6]; #Removes unique, database-specific keys
unique_Tmelastomoides <- unique(scrubbed_Tmelastomoides[,-4]); #Removes duplicate points (not considering data provider)

#Remove points with no environmental data - dup with script 1
terrestrialAltitude <- raster("./Lab4/terrestrialAltitude.asc")
south_america <- readOGR("./Lab4/joined/joined.shp")

#crop terrestrialAltitude to south america
crop_terrAlt <- crop(terrestrialAltitude, extent(south_america))
extent(south_america)
Tmel_Extract <- extract(crop_terrAlt, sapply(unique_Tmelastomoides[2:3], as.numeric))

Tmel_Extract<- cbind(unique_Tmelastomoides, Tmel_Extract)

clean_Tmel_Extract <- Tmel_Extract[complete.cases(Tmel_Extract[,5]),]

#Reduce points to resolution of environmental data - dup with script 1
rasterResolution <- max(res(terrestrialAltitude))

while(min(nndist(clean_Tmel_Extract[,2:3])) < rasterResolution){
  nnD <- nndist(clean_Tmel_Extract[,2:3])
  clean_Tmel_Extract <- clean_Tmel_Extract[-(which(min(nnD) == nnD)[1]),]
}
row.names(clean_Tmel_Extract) <- seq(nrow(clean_Tmel_Extract))

#Plot the results to see how things shook out.
plot(crop_terrAlt, main="Altitude", col = gray.colors(10, start = 0.3, end = 0.9, gamma = 2.2, alpha = NULL))
#Plot all points
points(spocc_Tmelastomoides_DF[2:3], pch = 16, cex = 1)
#Plot only cleanest dataset
points(clean_Tmel_Extract[,2:3], pch = 16, cex = 1, col = "red")
legend(-175, 90, c("Raw T melastomoides", "Clean T melastomoides"), pch = c(16,16), col=c("black", "red"))

#Save clean data
write.csv(clean_Tmel_Extract[,1:4], "./Lab4/clean_Tmel_PointsSPOCC.csv", row.names = F)
