library(tidyverse)
library(readxl)
library(rsdmx)
library(jsonlite)
library(IMFData)
library(rvest)
library(lubridate)
library(magrittr)
library(siebanxicor)
library(httr)
# library(archive)
library(seasonal)

excel_directory <- "../xlsx/"

catalogue <- read_excel(paste0(excel_directory,"Catalogue.xlsx"))
n_groups <- catalogue %>% slice(n()) %>% pull(download_group)
country_conv <- read_excel(paste0(excel_directory, "Country codes.xlsx"))

last_update_file <- sort(list.files("output/", pattern = "^.*_orig_.*\\.csv$"), decreasing = T)[1]
last_update <- read.csv(paste0("Output/", last_update_file))

source("Source/collect_data.R")
database <- collect_data(start = as.Date("2002-01-01"), end = as.Date("2020-04-01"), 
                         cat = catalogue, countries = country_conv)
write.csv(database$database, paste0("Output/database_orig_", format(today(), "%Y%m%d"), ".csv"), row.names = F)
write.csv(database$log, paste0("Output/!logs/download_log_", format(today(), "%Y%m%d"), ".csv"), row.names = F)

source("Source/compare_data.R")
compare <- compare_data(last_update, database$database, sensitivity = 0.02)
write.csv(compare$changelog, paste0("Output/!logs/compare_log_", format(today(), "%Y%m%d"), ".csv"), row.names = F)

source("Source/transform_data.R")
database_tf <- transform_data(database$database, cat = catalogue)
write.csv(database_tf$data_sa, paste0("Output/database_sa_", format(today(), "%Y%m%d"), ".csv"), row.names = F)
write.csv(database_tf$data_tf, paste0("Output/database_tf_", format(today(), "%Y%m%d"), ".csv"), row.names = F)

source("Source/prepare_data.R")
output <- prepare_data(database_tf$data_tf,
                       start = as.Date("2003-01-01"), end = as.Date("2020-12-01"))
write.csv(output$data_nowcast_goods_value, paste0("Output/!forOctave/value_", format(today(), "%Y%m%d"), ".csv"), row.names = F)
write.csv(output$data_nowcast_goods_volume, paste0("Output/!forOctave/volume_", format(today(), "%Y%m%d"), ".csv"), row.names = F)
write.csv(output$data_nowcast_services, paste0("Output/!forOctave/services_", format(today(), "%Y%m%d"), ".csv"), row.names = F)