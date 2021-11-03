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

query <- 'SELECT DISTINCT *
          FROM combined_herbarium_occurrences'

#Turn query into table and download
results_of_query <- dbGetQuery(conn, query)



