library(tidyverse)
library(hash)
library(rsdmx)
library(jsonlite)
library(httr)
library(readxl)
library(siebanxicor)
library(IMFData)
rm(list=ls())

# potential function parameters
helper_directory <- "../helper/"

start_date <- as.Date("2002-01-01")
end_date <- as.Date("2020-04-01")

catalog <- read_csv(paste0(helper_directory,"catalog.csv"))
countries <- read_csv(paste0(helper_directory, "country_codes.csv"))
historical <- read_csv(paste0("../output/historical.csv"))

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
begin_path <- "get_api/"
files <- list.files(begin_path)
files <- sapply(files, function(x) paste0(begin_path, x))
sapply(files, source)
# generating dictionary from catalog of api calls
data_hash <- gen_data_hash(catalog)

# function to generate the table of a g
get_group <- function (g) {
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
  } else if (data_source == "boj") {
    tmp <- get_boj(url, catalog, g, start_date, end_date)
  } else if (data_source == "ecb") {
    tmp <- get_single_api(url, catalog, g, which_time, data_source, start_date, end_date, "obsTime", "obsValue")
  } else if (data_source == "wto") {
    tmp <- get_single_api(url, catalog, g, which_time, data_source, start_date, end_date, "date", "Dataset.Value")
  } else if (data_source == "cbp") {
    tmp <- get_cpb(url, catalog, g, countries, start_date, end_date)
  } else if (data_source == "banxico")  {
    tmp <- get_banxico(url, catalog, g, start_date, end_date)
  } else if (data_source == "ec") {
    tmp <- get_ec(url, catalog, g, start_date, end_date)
  } else if (data_source == "rwi/isl") {
    tmp <- get_rwi(url, catalog, g, start_date, end_date)
  } else if (data_source == "meti") {
    tmp <- get_meti(url, catalog, g, start_date, end_date)
  } else if (data_source == "ons") {
    tmp <- get_ons(url, catalog, g, start_date, end_date)
  } else if (data_source == "ibge") {
    tmp <- get_ibge(url, catalog, g, start_date, end_date)
  } else if (data_source == "nso_tr") {
    tmp <- get_nso_tr(url, catalog, g, start_date, end_date)
  } else if (data_source == "nso_sg") {
    tmp <- get_nso_sg(url, catalog, g, start_date, end_date)
  } else if (data_source == "lhr") {
    tmp <- get_lhr(url, catalog, g, start_date, end_date)
  }
    
  return(tmp)
}
# -1 for the nbs double entry, try multiple times
groups <- 1:(length(data_hash)-1)
n_tries <- 1
while (n_tries <= 5) {
  for (g in groups) {
    skip <- FALSE
    # only try if not already updated
    if (log %>% filter(download_group == g) %>% select(status) %>% slice(1) %>% pull == 1){
      tmp <- get_group(g)
      tryCatch({
        database <- cbind(database,tmp)
        log[log$download_group == g, "status"] <- 0
      }, error = function(e) {
        skip <<- TRUE
      })
      if (skip) {next}
    }
  }
  n_tries <- n_tries + 1
}