#Query database with SQL for each species

#load libraries
library(RSQLite)

#Make new database object for Tibouchina data
Tibouchina_data <- "./Databases/Tibouchina_database.sqlite"

#Connect to database
conn <- dbConnect(drv = SQLite(), dbname= Tibouchina_data)
