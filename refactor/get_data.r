library(tidyverse)
library(hash)
library(rsdmx)
library(jsonlite)
library(httr)
library(IMFData)
rm(list=ls())

# potential function parameters
helper_directory <- "helper/"

start <- as.Date("2002-01-01")
end <- as.Date("2020-04-01")

catalog <- read_csv(paste0(helper_directory,"catalog.csv"))
countries <- read_csv(paste0(helper_directory, "country_codes.csv"))
historical <- read_csv(paste0("output/historical.csv"))

# getting dates/quarters from start/end date
start_date <- as.Date(start, format = "%Y-%m-%d")
end_date <- as.Date(end, format = "%Y-%m-%d")

# initializing the database
database <- seq(from = start_date, to = end_date, by = "month") %>%
  data.frame %>%
  rename(date=1)

# initializing a log to keep track of download status
log <- catalog %>%
  select(code, download_group) %>%
  mutate(status=1)

###
### Getting data
source("src/get_api.r")
# generating dictionary from catalog of api calls
data_hash <- gen_data_hash(catalog)

# -1 for the nbs double entry
for (g in 1:(length(data_hash)-1)) {
  print(paste("Fetching group", g))
  
  url <-data_hash[[as.character(g)]][1]
  which_time <-data_hash[[as.character(g)]][2]
  data_source <- data_hash[[as.character(g)]][3]
  
  # getting api data
  if (data_source %in% c("oecd", "eurostat", "imf")) {
    tmp <- get_api(url, catalog, g, countries, which_time, data_source, start_date, end_date)
  } else if (data_source == "fred") {
    tmp <- get_fred(url, catalog, g, start_date, end_date)
  } else if (data_source == "nbs") {
    # if nbs is split in 2 tables, get the 2nd too
    if (has.key(paste0(as.character(g),"b"), data_hash)) {
      url2 <- data_hash[[paste0(as.character(g),"b")]][1]
      tmp <- get_nbs_double(url, url2, catalog, g, start_date, end_date)
    } else {
      tmp <- get_nbs(url, catalog, g, start_date, end_date)
    }
  } else if (data_source == "hk nso") {
    tmp <- get_hk_nso(url, catalog, g, start_date, end_date, historical)
  }
  
  database <- cbind(database,tmp)
  log[log$download_group == g, "status"] <- 0
}