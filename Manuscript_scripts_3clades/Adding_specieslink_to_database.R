#New database with species link data
library(dismo) 
library(RSQLite) 
library(plyr) 
library(scrubr)
library(stringr)
library(tidyr)

#Connect to database
Tibouchina_database <- "./Databases/Tibouchina_database_spLinktoo.sqlite"

#Connect to database
conn <- dbConnect(drv = SQLite(), dbname= Tibouchina_database)

#Read csv file with list of taxa of interest
taxa <- read.csv("./Databases/Tibouchina_species_comprehensive.csv", stringsAsFactors = FALSE)

#Assign the list of species epithet names
species_names <- taxa$Genus_species

#Download data from specieslink
#Downloaded all data for Tibouchina from speciesLink via the website, saved as speciesLink_data.txt

#Read data  (downloaded from SpeciesLink)
spLink_occurrences <- read.delim("./Data/3_clades/speciesLink_data.txt", stringsAsFactors = FALSE)

#Convert date to standard format
#pad date with zeros
spLink_occurrences$monthcollected <- str_pad(spLink_occurrences$monthcollected, 2, side = "left", pad = "0")
spLink_occurrences$daycollected <- str_pad(spLink_occurrences$daycollected, 2, side = "left", pad = "0")

#Create date column
spLink_occurrences_date <- date_create(spLink_occurrences, yearcollected, monthcollected, daycollected)
spLink_occurrences_date$date <- as.Date(spLink_occurrences_date$date, "%Y-%m-%d")

#Convert to standard format
spLink_occurrences_date2 <- date_standardize(spLink_occurrences_date, "%d%b%Y", date_column = "date")
spLink_occurrences_date2 <- coord_incomplete(spLink_occurrences_date2)
spLink_occurrences_date2 <- coord_unlikely(spLink_occurrences_date2)

#Read list of taxa of interest
#species_names <- read.csv("./Data/Tibouchina_species.csv", stringsAsFactors = FALSE)

#Add column with species of interest or not
for (i in 1:nrow(spLink_occurrences_date2)){
  if (spLink_occurrences_date2$scientificname[i] %in% species_names == TRUE) {
    spLink_occurrences_date2$interest[i] <- "yes"
  } else {
    spLink_occurrences_date2$interest[i] <- "no"
  }
}

#Add columns of nulls to match gbif data and downloaded from spLink
spLink_occurrences_date2$site <- "spLink"
spLink_occurrences_date2$collectionid <- "NA"
spLink_occurrences_date2$crawlId <- "NA"
spLink_occurrences_date2$elevationAccuracy <- "NA"
spLink_occurrences_date2$eventDate <- "NA"
spLink_occurrences_date2$gbifID <- "NA"
spLink_occurrences_date2$geodeticDatum <- "NA"
spLink_occurrences_date2$recordedby <- "NA"
spLink_occurrences_date2$recordnumber <- "NA"
spLink_occurrences_date2$ISO2 <- "NA"
spLink_occurrences_date2$name <- "NA"
spLink_occurrences_date2$taxonID <- "NA"

#Select specific columns to match gbif data
sp_to_write <- spLink_occurrences_date2[,c("catalognumber", "collectioncode", "collectionid", "coordinateprecision", "country", "crawlId", "daycollected", "maximumelevation", "elevationAccuracy", "eventDate", "family", "notes", "fieldnumber", "gbifID", "genus", "geodeticDatum", "identifiedby", "individualcount", "subspecies", "institutioncode", "ISO2", "latitude", "locality", "longitude", "monthcollected", "recordedby", "recordnumber", "name", "scientificname", "species", "taxonID", "yearcollected", "date", "interest", "site")]

#Change column names to same as gbif table
query2 <- "SELECT DISTINCT *
FROM combined_herbarium_occurrences"

#Turn query into table and download
results_of_query <- tbl(conn, sql(query2)) %>%
  collect()

column_names <- colnames(results_of_query)

#check for matches
colnames(sp_to_write)[-which(colnames(sp_to_write) %in% column_names)]

column_names[-which(column_names %in% colnames(sp_to_write))]

#rename columns to match
colnames(sp_to_write)[which(colnames(sp_to_write) == "catalognumber")] <- "catalogNumber"
colnames(sp_to_write)[which(colnames(sp_to_write) == "collectioncode")] <- "collectionCode"
colnames(sp_to_write)[which(colnames(sp_to_write) == "collectionid")] <- "collectionID"
colnames(sp_to_write)[which(colnames(sp_to_write) == "coordinateprecision")] <- "coordinateUncertaintyInMeters"
colnames(sp_to_write)[which(colnames(sp_to_write) == "daycollected")] <- "day"
colnames(sp_to_write)[which(colnames(sp_to_write) == "notes")] <- "fieldNotes"
colnames(sp_to_write)[which(colnames(sp_to_write) == "fieldnumber")] <- "fieldNumber"
colnames(sp_to_write)[which(colnames(sp_to_write) == "identifiedby")] <- "identifiedBy"
colnames(sp_to_write)[which(colnames(sp_to_write) == "subspecies")] <- "infraspecificEpithet"
colnames(sp_to_write)[which(colnames(sp_to_write) == "institutioncode")] <- "institutionCode"
colnames(sp_to_write)[which(colnames(sp_to_write) == "latitude")] <- "lat"
colnames(sp_to_write)[which(colnames(sp_to_write) == "longitude")] <- "lon"
colnames(sp_to_write)[which(colnames(sp_to_write) == "monthcollected")] <- "month"
colnames(sp_to_write)[which(colnames(sp_to_write) == "maximumelevation")] <- "elevation"
colnames(sp_to_write)[which(colnames(sp_to_write) == "recordedby")] <- "recordedBy"
colnames(sp_to_write)[which(colnames(sp_to_write) == "recordnumber")] <- "recordNumber"
colnames(sp_to_write)[which(colnames(sp_to_write) == "scientificname")] <- "scientificName"
colnames(sp_to_write)[which(colnames(sp_to_write) == "yearcollected")] <- "year"
colnames(sp_to_write)[which(colnames(sp_to_write) == "date")] <- "verbatimEventDate"
colnames(sp_to_write)[which(colnames(sp_to_write) == "individualcount")] <- "individualCount"
colnames(sp_to_write)[which(colnames(sp_to_write) == "site")] <- "source"

#remove name, interest columns
sp_to_write <- sp_to_write[,-c(28, 34)]

#Upload to database

#Write taxa table to database
dbWriteTable(conn,"spLink", sp_to_write, overwrite=TRUE)

dbWriteTable(conn, "combined_herbarium_occurrences", sp_to_write, append = TRUE)

#Disconnect database 
dbDisconnect(conn)
