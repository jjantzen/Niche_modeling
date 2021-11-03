#Downloading data from idigbio and gbif

#Load libraries
library(rgbif) 
library(dismo) 
library(RSQLite) 
library(plyr) 
library(ridigbio)
library(spocc)
library(scrubr)
library(stringr)
library(tidyr)

#Make new database for Tibouchina data
Tibouchina_database <- "./Databases/Tibouchina_database.sqlite"

#Connect to database
conn <- dbConnect(drv = SQLite(), dbname= Tibouchina_database)

#Read csv file with list of taxa of interest
taxa <- read.csv("./Databases/Tibouchina_species_comprehensive.csv", stringsAsFactors = FALSE)

#Assign the list of species epithet names
species_names <- taxa$Genus_species

#Write taxa table to database
dbWriteTable(conn,"taxa", taxa, overwrite=TRUE)

#Download from idigbio

#Make empty dataframe
idigbio_occurrences <- data.frame()

#Run for loop for each species name in list of species names
for (i in 1:length(species_names)) {
  #go to gbif and download occurrences with geographic references (removing zero lat and long) for species of Tibouchina
  DL <- occ(query = taxa$Genus_species[i], from = 'idigbio', has_coords = T, idigbioopts = list(fields = 'all'))
  #turn occ data into df
  #turn DL into dataframe
  if (DL$idigbio$meta$returned > 0) {
    #DL <- fixnames(DL, how = "query")
    DL_df <- occ2df(DL)
    #DL_df <- date_standardize(DL_df, "%d%b%Y")
    #DL_df <- coord_incomplete(DL_df)
    #DL_df <- coord_unlikely(DL_df)
    
    #check if dataframe is empty
    if (nrow(idigbio_occurrences) == 0) {
      idigbio_occurrences <- data.frame(DL_df)
    } else {
      #If dataframe is not empty, add rows, filling unmatched columns with NAs
      idigbio_occurrences <- rbind.fill(idigbio_occurrences, DL_df)
    }
  } else {
    DL_df <- DL
  }
}


#Get list of columns
colnames(idigbio_occurrences)

#Download from gbif

#Make empty dataframe
gbif_occurrences <- data.frame()

#Run for loop for each species name in list of species names
for (i in 1:length(species_names)) {
  #go to gbif and download occurrences without geographic references for species of Tibouchina
  DL <- gbif(taxa$Genus_species[i])
  #Check for date column
  if (is.null(DL) == FALSE){
    if ("eventDate" %in% colnames(DL)){
      #convert date to standard format
      DL$eventDate <- as.Date(DL$eventDate, "%Y-%m-%d")
      #DL <- date_standardize(DL, "%d%b%Y", date_column = "eventDate") 
    } else {
      DL$eventDate <- NULL
    }
    #if ("lat" %in% colnames(DL)){
    #  DL <- coord_incomplete(DL)
    #  DL <- coord_unlikely(DL)
    #} 
    
    #check if dataframe is empty
    if (nrow(gbif_occurrences) == 0) {
      gbif_occurrences <- data.frame(DL)
    } else {
      #If dataframe is not empty, add rows, filling unmatched columns with NAs
      gbif_occurrences <- rbind.fill(gbif_occurrences, DL)
    }
  } else {
    DL <- DL
  }
}

#Add columns saying these are downloaded from gbif
gbif_occurrences$source <- "gbif"

column_names <- colnames(gbif_occurrences)

#Add columns to idigbio data to have matching columns
colnames(idigbio_occurrences) <- c("species", "longitude", "latitude", "source", "date", "key")
for (i in 1:nrow(idigbio_occurrences)){
  date_occurrences <- idigbio_occurrences %>%
    separate(date, sep="-", into = c("year", "month", "day"))
}
idigbio_occurrences <- date_occurrences
# #Add speciesname authority and barcode columns
# gbif_occurrence$barcode <- NA
# gbif_occurrences$scientificNameAuthoriy <- NA
# 
# idigbio_occurrence$barcode <- NA
# idigbio_occurrences$scientificNameAuthoriy <- NA

#Add columns to gbif dataset
gbif_occurrences$scientificNameAuthor <- NA
gbif_occurrences$elevationMax <- NA
gbif_occurrences$barcode <- NA
gbif_occurrences$elevationUnit <- NA
gbif_occurrences$latitudeMaxq <- NA
gbif_occurrences$longitudeMaxq <- NA

#Check date for NA
unique(idigbio_occurrences$month)
idigbio_occurrences[which(is.na(idigbio_occurrences$month)),]

#Convert idigbio dates to number # not working
idigbio_occurrences$month <- as.integer(idigbio_occurrences$month)
#idigbio_occurrences$month <- match(idigbio_occurrences$month,month.abb)
str(gbif_occurrences$month)
str(idigbio_occurrences$month)

#Check latlong for na
unique(idigbio_occurrences$latitude) %>%  sort()

#Check elevation for na and set some to na
unique(gbif_occurrences$elevation) %>% sort()
gbif_occurrences$elevation[which(gbif_occurrences$elevation == -4999.50 | gbif_occurrences$elevation == 0.00 | gbif_occurrences$elevation == 9999.00)] <- NA

#Check species vs specificEpithet
unique(gbif_occurrences$specificEpithet)

#write the occurrences table to database
dbWriteTable(conn, "gbif_occurrences", gbif_occurrences, overwrite=TRUE)

#write the occurrences table to database
dbWriteTable(conn, "idigbio_occurrences", idigbio_occurrences, overwrite = TRUE)

colnames(idigbio_occurrences)[2] <- "lon"
colnames(idigbio_occurrences)[3] <- "lat"

#Write both to complete database
dbWriteTable(conn, "combined_herbarium_occurrences", gbif_occurrences, overwrite = TRUE)
dbWriteTable(conn, "combined_herbarium_occurrences", idigbio_occurrences, append = TRUE)

#Disconnect database 
dbDisconnect(conn)
