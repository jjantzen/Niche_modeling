#Query database with SQL for each species

#load libraries
library(RSQLite)
library(dplyr)
library(dbplyr)
library(scrubr)

#Make new database object for Tibouchina data
Tibouchina_data <- "./Databases/Tibouchina_database.sqlite"

#Connect to database
conn <- dbConnect(drv = SQLite(), dbname= Tibouchina_data)

#Query phrase

query <- "SELECT DISTINCT specificEpithet, day, month, year, lat, lon
FROM combined_herbarium_occurrences"

#Turn query into table and download
results_of_query <- tbl(conn, sql(query)) %>% 
  collect()

#Make lat long numeric
results_of_query$lat <- as.numeric(results_of_query$lat)
results_of_query$lon <- as.numeric(results_of_query$lon)

#Filter based on presence of lat long
filtered_results <- results_of_query %>%
  filter(!is.na(lat), !is.na(lon))

filtered_results_df <- as.data.frame(filtered_results)

#Remove incomplete records and unlikely coordinates
scrubbed_results <- coord_incomplete(filtered_results_df, lat = "lat", lon = "lon")
scrubbed_results <- coord_unlikely(scrubbed_results, lat = "lat", lon = "lon")

unique(scrubbed_results[61:80,])

#Remove duplicate dates and localities
unique_results <- unique(scrubbed_results)

unique_results

#Write data to file
write.csv(unique_results, "./Data/3_clades/Tib_all_unique.csv", row.names = FALSE)
