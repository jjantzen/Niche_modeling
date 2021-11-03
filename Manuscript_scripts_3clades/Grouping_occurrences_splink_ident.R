#group points into clades then reduce resolution and find outliers

#list species of each clade
north <- c("striphnocalyx", "fraterna", "duidae", "bipenicillata", "karstenii", "mathaei", "catharinae", "dissitiflora", "kunhardtii", "sipapoana")
wide <- c("spruceana", "aspera")
#south_long <- c("Sp6", "Angustifolia", "Sp9", "Papyrus", "Melastomoides", "Bruniana", "Aegopogon", "Sp7", "Barbigera", "Pogonanthera", "Llanorum", "Edmundoi", "Albescens", "Verticillaris", "Exasperata", "Sp8", "Sp5", "Nigricans", "Sp3")
south <- c("papyrus", "melastomoides", "bruniana", "aegopogon", "barbigera", "pogonanthera", "llanorum", "edmundoi", "albescens", "verticillaris", "exasperata", "nigricans")

#read file
splink <- read.csv("./Data/3_clades/Tib_all_cleaned_splink_ident.csv", stringsAsFactors = FALSE)
ident <- read.csv("./Data/3_clades/Tib_all_cleaned_ident.csv", stringsAsFactors = FALSE)
original <- read.csv("./Data/3_clades/Tib_all_cleaned_original.csv", stringsAsFactors = FALSE)


#split occurrences by clade
north_occs_splink <- splink[which(splink$name %in% north),]
south_occs_splink <- splink[which(splink$name %in% south),]
wide_occs_splink <- splink[which(splink$name %in% wide),]

north_occs_ident <- ident[which(ident$name %in% north),]
south_occs_ident <- ident[which(ident$name %in% south),]
wide_occs_ident <- ident[which(ident$name %in% wide),]

north_occs_original <- original[which(original$name %in% north),]
south_occs_original <- original[which(original$name %in% south),]
wide_occs_original <- original[which(original$name %in% wide),]

#Get resolution of raster of environmental data
terrestrialAltitude <- raster("./Layers/terrestrialAltitude.asc")
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
crs(terrestrialAltitude) <- crs.geo

rasterResolution <- max(res(terrestrialAltitude))
rasterResolution

#Reduce resolution
while(min(nndist(north_occs_splink[,2:3])) < rasterResolution){
  nnD <- nndist(north_occs_splink[,2:3])
  north_occs_splink <- north_occs_splink[-(which(min(nnD) == nnD)[1]),]
}

north_occs_splink

while(min(nndist(north_occs_ident[,2:3])) < rasterResolution){
  nnD <- nndist(north_occs_ident[,2:3])
  north_occs_ident <- north_occs_ident[-(which(min(nnD) == nnD)[1]),]
}

north_occs_ident

while(min(nndist(north_occs_original[,2:3])) < rasterResolution){
  nnD <- nndist(north_occs_original[,2:3])
  north_occs_original <- north_occs_original[-(which(min(nnD) == nnD)[1]),]
}

north_occs_original

while(min(nndist(south_occs_splink[,2:3])) < rasterResolution){
  nnD <- nndist(south_occs_splink[,2:3])
  south_occs_splink <- south_occs_splink[-(which(min(nnD) == nnD)[1]),]
}

south_occs_splink

while(min(nndist(south_occs_ident[,2:3])) < rasterResolution){
  nnD <- nndist(south_occs_ident[,2:3])
  south_occs_ident <- south_occs_ident[-(which(min(nnD) == nnD)[1]),]
}

south_occs_ident

while(min(nndist(south_occs_original[,2:3])) < rasterResolution){
  nnD <- nndist(south_occs_original[,2:3])
  south_occs_original <- south_occs_original[-(which(min(nnD) == nnD)[1]),]
}

south_occs_original

while(min(nndist(wide_occs_splink[,2:3])) < rasterResolution){
  nnD <- nndist(wide_occs_splink[,2:3])
  wide_occs_splink <- wide_occs_splink[-(which(min(nnD) == nnD)[1]),]
}

wide_occs_splink

while(min(nndist(wide_occs_ident[,2:3])) < rasterResolution){
  nnD <- nndist(wide_occs_ident[,2:3])
  wide_occs_ident <- wide_occs_ident[-(which(min(nnD) == nnD)[1]),]
}

wide_occs_ident

while(min(nndist(wide_occs_original[,2:3])) < rasterResolution){
  nnD <- nndist(wide_occs_original[,2:3])
  wide_occs_original <- wide_occs_original[-(which(min(nnD) == nnD)[1]),]
}

wide_occs_original

#Set rownames of clean dataset
row.names(wide_occs_splink) <- seq(nrow(wide_occs_splink))
row.names(south_occs_splink) <- seq(nrow(south_occs_splink))
row.names(north_occs_splink) <- seq(nrow(north_occs_splink))

row.names(wide_occs_ident) <- seq(nrow(wide_occs_ident))
row.names(south_occs_ident) <- seq(nrow(south_occs_ident))
row.names(north_occs_ident) <- seq(nrow(north_occs_ident))

row.names(wide_occs_original) <- seq(nrow(wide_occs_original))
row.names(south_occs_original) <- seq(nrow(south_occs_original))
row.names(north_occs_original) <- seq(nrow(north_occs_original))


#Check for outliers by plotting
plot_wide_o <- wide_occs_original
plot_north_o <- north_occs_original
plot_south_o <- south_occs_original


plot_wide_i <- wide_occs_ident
plot_north_i <- north_occs_ident
plot_south_i <- south_occs_ident


plot_wide_s <- wide_occs_splink
plot_north_s <- north_occs_splink
plot_south_s <- south_occs_splink


coordinates(plot_wide_o) <- c("lon", "lat")
coordinates(plot_north_o) <- c("lon", "lat")
coordinates(plot_south_o) <- c("lon", "lat")

coordinates(plot_wide_i) <- c("lon", "lat")
coordinates(plot_north_i) <- c("lon", "lat")
coordinates(plot_south_i) <- c("lon", "lat")

coordinates(plot_wide_s) <- c("lon", "lat")
coordinates(plot_north_s) <- c("lon", "lat")
coordinates(plot_south_s) <- c("lon", "lat")


proj4string(plot_wide_i) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
proj4string(plot_north_i) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
proj4string(plot_south_i) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

proj4string(plot_wide_o) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
proj4string(plot_north_o) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
proj4string(plot_south_o) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

proj4string(plot_wide_s) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
proj4string(plot_north_s) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
proj4string(plot_south_s) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

south_america <- readOGR("./Layers/joined.shp")
crs(south_america) <- crs.geo

plot(south_america)
plot(plot_wide_o, add=TRUE, col = "purple", shape = 17)
plot(plot_north_o, add=TRUE, col = "purple", shape = 18)
plot(plot_south_o, add=TRUE, col = "purple", shape = 19)

plot(plot_wide_i, add=TRUE, col = "red", shape = 17)
plot(plot_north_i, add=TRUE, col = "red", shape = 18)
plot(plot_south_i, add=TRUE, col = "red", shape = 19)

plot(plot_wide_s, add=TRUE, col = "green", shape = 17)
plot(plot_north_s, add=TRUE, col = "red", shape = 18)
plot(plot_south_s, add=TRUE, col = "purple", shape = 19)


############

#To order points to remove specific outliers
ordered_points <- wide_occs_splink %>% 
  arrange(lat)
head(ordered_points)
tail(ordered_points)

ordered_points_removed <- ordered_points#[-c(1,2),]

ordered_points_removed <- ordered_points_removed %>% 
  arrange(lon)

write.csv(ordered_points_removed[,1:3], "./Data/3_clades/Wide_clade_cleaned_occurrences_splin_ident.csv", row.names = F)


ordered_points <- north_occs_splink %>% 
  arrange(lat)
ordered_points[c(nrow(ordered_points)-20:nrow(ordered_points)),]
head(ordered_points)

ordered_points_removed <- ordered_points[-1,]

ordered_points_removed <- ordered_points_removed %>% 
  arrange(lon)

write.csv(ordered_points_removed[,1:3], "./Data/3_clades/North_clade_cleaned_occurrences_splin_ident.csv", row.names = F)


ordered_points <- south_occs_splink %>% 
  arrange(lat)
ordered_points[c(nrow(ordered_points)),]
head(ordered_points)
ordered_points[c(180:194),]

ordered_points_removed <- ordered_points[-c(1),]
tail(ordered_points_removed)

ordered_points_removed <- ordered_points_removed %>% 
  arrange(lon)

write.csv(ordered_points_removed[,1:3], "./Data/3_clades/South_clade_cleaned_occurrences_splin_ident.csv", row.names = F)
