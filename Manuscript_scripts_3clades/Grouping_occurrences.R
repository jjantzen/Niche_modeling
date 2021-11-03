#group points into clades then reduce resolution and find outliers

#list species of each clade
north <- c("striphnocalyx", "fraterna", "duidae", "bipenicillata", "karstenii", "mathaei", "catharinae", "dissitiflora", "kunhardtii", "sipapoana")
wide <- c("spruceana", "aspera")
#south_long <- c("Sp6", "Angustifolia", "Sp9", "Papyrus", "Melastomoides", "Bruniana", "Aegopogon", "Sp7", "Barbigera", "Pogonanthera", "Llanorum", "Edmundoi", "Albescens", "Verticillaris", "Exasperata", "Sp8", "Sp5", "Nigricans", "Sp3")
south <- c("papyrus", "melastomoides", "bruniana", "aegopogon", "barbigera", "pogonanthera", "llanorum", "edmundoi", "albescens", "verticillaris", "exasperata", "nigricans")

#read file
all_points <- read.csv("./Data/3_clades/Tib_all_cleaned.csv", stringsAsFactors = FALSE)

#split occurrences by clade
north_occs <- all_points[which(all_points$name %in% north),]
south_occs <- all_points[which(all_points$name %in% south),]
wide_occs <- all_points[which(all_points$name %in% wide),]

#Get resolution of raster of environmental data
terrestrialAltitude <- raster("./Layers/terrestrialAltitude.asc")
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
crs(terrestrialAltitude) <- crs.geo

rasterResolution <- max(res(terrestrialAltitude))
rasterResolution

#Reduce resolution
while(min(nndist(north_occs[,2:3])) < rasterResolution){
  nnD <- nndist(north_occs[,2:3])
  north_occs <- north_occs[-(which(min(nnD) == nnD)[1]),]
}

north_occs

while(min(nndist(south_occs[,2:3])) < rasterResolution){
  nnD <- nndist(south_occs[,2:3])
  south_occs <- south_occs[-(which(min(nnD) == nnD)[1]),]
}

south_occs

while(min(nndist(wide_occs[,2:3])) < rasterResolution){
  nnD <- nndist(wide_occs[,2:3])
  wide_occs <- wide_occs[-(which(min(nnD) == nnD)[1]),]
}

wide_occs

#Set rownames of clean dataset
row.names(wide_occs) <- seq(nrow(wide_occs))
row.names(south_occs) <- seq(nrow(south_occs))
row.names(north_occs) <- seq(nrow(north_occs))


#Check for outliers by plotting
plot_wide <- wide_occs
plot_north <- north_occs
plot_south <- south_occs
coordinates(plot_wide) <- c("lon", "lat")
coordinates(plot_north) <- c("lon", "lat")
coordinates(plot_south) <- c("lon", "lat")
proj4string(plot_wide) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
proj4string(plot_north) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
proj4string(plot_south) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

south_america <- readOGR("./Layers/joined.shp")
crs(south_america) <- crs.geo

plot(south_america)
plot(plot_wide, add=TRUE, col = "purple")
plot(plot_north, add=TRUE, col = "red")
plot(plot_south, add=TRUE, col = "green")


############

#To order points to remove specific outliers
ordered_points <- wide_occs %>% 
  arrange(lat)
head(ordered_points)
tail(ordered_points)

ordered_points_removed <- ordered_points[-c(1,2),]

ordered_points_removed <- ordered_points_removed %>% 
  arrange(lon)

write.csv(ordered_points_removed[,1:3], "./Data/3_clades/Wide_clade_cleaned_occurrences.csv", row.names = F)


ordered_points <- north_occs %>% 
  arrange(lat)
ordered_points[c(nrow(ordered_points)-20:nrow(ordered_points)),]
head(ordered_points)

ordered_points_removed <- ordered_points[-c(1:4),]

ordered_points_removed <- ordered_points_removed %>% 
  arrange(lon)

write.csv(ordered_points_removed[,1:3], "./Data/3_clades/North_clade_cleaned_occurrences.csv", row.names = F)


ordered_points <- south_occs %>% 
  arrange(lat)
ordered_points[c(nrow(ordered_points)),]
head(ordered_points)
ordered_points[c(300:318),]

ordered_points_removed <- ordered_points[-c(1),]
tail(ordered_points_removed)

ordered_points_removed <- ordered_points_removed %>% 
  arrange(lon)

write.csv(ordered_points_removed[,1:3], "./Data/3_clades/South_clade_cleaned_occurrences.csv", row.names = F)
