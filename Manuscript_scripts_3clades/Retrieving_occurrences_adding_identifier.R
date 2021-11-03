#Query database with SQL for each species

#load libraries
library(RSQLite)
library(dbplyr)
library(scrubr)
library(dplyr)

#Make new database object for Tibouchina data
Tibouchina_data <- "./Databases/Tibouchina_database_spLinktoo.sqlite"

#Connect to database
conn <- dbConnect(drv = SQLite(), dbname= Tibouchina_data)

#Query phrase

query <- "SELECT DISTINCT specificEpithet, day, month, year, lat, lon, identifiedBy
FROM combined_herbarium_occurrences
WHERE  (identifiedBy LIKE '%Wurdack%' OR identifiedBy LIKE '%Meyer%' OR (identifiedBy LIKE '%GuimarÃ£es%' AND identifiedBy NOT LIKE '%R.%' AND identifiedBy NOT LIKE '%E.F.%') OR (identifiedBy LIKE '%Guimarães%' AND identifiedBY NOT LIKE '%Elsie%' AND identifiedBy NOT LIKE '%R.P.%') OR identifiedBy LIKE '%Guimaraes%' OR (identifiedBy LIKE '%Romero%' AND identifiedBy NOT LIKE '%Carlos%') OR identifiedBy LIKE '%Brade%' OR identifiedBy LIKE '%Todzia%' OR (identifiedBy LIKE '%Freitas%' AND identifiedBy NOT LIKE '%J.G.%' AND identifiedBy NOT LIKE '%J. G.%' AND identifiedBy NOT LIKE '%JG%' AND identifiedBy NOT LIKE '%G.A.%') AND identifiedBy NOT LIKE '%Chiea%')"

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
write.csv(unique_results, "./Data/3_clades/Tib_all_unique_with_identifier_sp_link.csv", row.names = FALSE)
