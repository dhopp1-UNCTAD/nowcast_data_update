library(tidyverse)
library(hash)
library(rsdmx)
library(jsonlite)
library(IMFData)
rm(list=ls())

# potential function parameters
helper_directory <- "helper/"

start <- as.Date("2002-01-01")
end <- as.Date("2020-04-01")

cat <- read_csv(paste0(helper_directory,"catalog.csv"))
countries <- read_csv(paste0(helper_directory, "country_codes.csv"))

# getting dates/quarters from start/end date
start_date <- as.Date(start, format = "%Y-%m-%d")
start_year <- format(start_date, "%Y")
start_quarter <- paste0(start_year, "-Q", max(1, floor(as.integer(format(start_date, "%m")) / 3)))
start_month <- format(start_date, "%m")

end_date <- as.Date(end, format = "%Y-%m-%d")
end_year <- format(end_date, "%Y")
end_month <- format(end_date, "%m")
if(as.integer(end_month) >= 3) {
  end_quarter <- paste0(end_year, "-Q", floor(as.integer(format(end_date, "%m")) / 3))
} else {
  end_quarter <- paste0(format(end_date %m-% months(12), "%Y"), "-Q4")
}

# initializing the database
database <- seq(from = start_date, to = end_date, by = "month") %>%
  data.frame %>%
  rename(date=1)

# initializing a log to keep track of download status
log <- cat %>%
  select(code, download_group) %>%
  mutate(status=1)

###
### Getting data
source("src/get_api.r")
data_hash <- hash()
# generating dictionary from catalog of api calls
# only do below 27 for now (where i have done)
for (i in unique(cat$download_group)) {
  if (i == "22b" | as.numeric(i) <= 27) {
    which_time <- cat %>% filter(download_group == i) %>% select(frequency) %>% slice(1) %>% pull
    source <- cat %>% filter(download_group == i) %>% select(source) %>% slice(1) %>% pull
    url <- cat %>% filter(download_group == i) %>% select(url) %>% slice(1) %>% pull
    if (source == "nbs") { url <- str_replace(url, "START_YEAR", start_year) }
    data_hash[[i]] <- c(url, which_time, source)
  }
}

# -1 for the nbs double entry
for (g in 1:(length(data_hash)-1)) {
  print(paste("Fetching group", g))
  
  url <-data_hash[[as.character(g)]][1]
  which_time <-data_hash[[as.character(g)]][2]
  data_source <- data_hash[[as.character(g)]][3]
  
  # getting url    
  if (data_source == "oecd") {
    start_url <- start_date
  } else if (data_source == "eurostat") {
    start_url <- start_year
  } else {
    start_url <- ""
  }
  url <- gen_url(url, start_url)
  
  # getting api data
  if (data_source %in% c("oecd", "eurostat", "imf")) {
    tmp <- get_api(url, cat, g, countries, which_time, data_source)
  } else if (data_source == "fred") {
    tmp <- get_fred(url, cat, g)
  } else if (data_source == "nbs") {
    # if nbs is split in 2 tables, get the 2nd too
    if (has.key(paste0(as.character(g),"b"), data_hash)) {
      url2 <- data_hash[[paste0(as.character(g),"b")]][1]
      tmp <- get_nbs_double(url, url2, cat, g)
    } else {
      tmp <- get_nbs(url, cat, g)
    }
  }
  
  database <- cbind(database,tmp)
  log[log$download_group == g, "status"] <- 0
}