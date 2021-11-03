#Query database with SQL for each species

#load libraries
library(RSQLite)
library(dplyr)
library(dbplyr)

#Make new database object for Tibouchina data
Tibouchina_data <- "./../../Database/Databases/Tibouchina_database.sqlite"

#Connect to database
conn <- dbConnect(drv = SQLite(), dbname= Tibouchina_data)

#Query phrase

query <- "SELECT DISTINCT specificEpithet, scientificName, day, month, year, latitude, longitude, catalogNumber, institutionCode, source
FROM combined_herbarium_occurrences
WHERE  (scientificName LIKE '%verticil%') AND (identifiedBy LIKE '%Wurdack%' OR identifiedBy LIKE '%Meyer%' OR (identifiedBy LIKE '%GuimarÃ£es%' AND identifiedBy NOT LIKE '%R.%' AND identifiedBy NOT LIKE '%E.F.%') OR (identifiedBy LIKE '%Guimarães%' AND identifiedBY NOT LIKE '%Elsie%' AND identifiedBy NOT LIKE '%R.P.%') OR identifiedBy LIKE '%Guimaraes%' OR (identifiedBy LIKE '%Romero%' AND identifiedBy NOT LIKE '%Carlos%') OR identifiedBy LIKE '%Brade%' OR identifiedBy LIKE '%Todzia%' OR (identifiedBy LIKE '%Freitas%' AND identifiedBy NOT LIKE '%J.G.%' AND identifiedBy NOT LIKE '%J. G.%' AND identifiedBy NOT LIKE '%JG%' AND identifiedBy NOT LIKE '%G.A.%') AND identifiedBy NOT LIKE '%Chiea%')"

#Turn query into table and download
results_of_query <- tbl(conn, sql(query)) %>% 
  collect()

results_of_query


#Make lat long numeric
results_of_query$latitude <- as.numeric(results_of_query$latitude)
results_of_query$longitude <- as.numeric(results_of_query$longitude)

#Filter based on presence of lat long
filtered_results <- results_of_query %>%
  filter(!is.na(latitude), !is.na(longitude))

filtered_results_df <- as.data.frame(filtered_results)

#Remove incomplete records and unlikely coordinates
scrubbed_results <- coord_incomplete(filtered_results_df)
scrubbed_results <- coord_unlikely(scrubbed_results)

scrubbed_results

#Remove duplicate dates and localities
unique_results <- unique(scrubbed_results)

unique_results

#Write data to file
write.csv(unique_results, "./Data/Occurrence_records_by_species/Tverticillaris_unique_detailed.csv", row.names = FALSE)


