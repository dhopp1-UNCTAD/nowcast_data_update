suppressPackageStartupMessages({
  library(tidyverse)
  library(hash)
  library(rsdmx)
  library(jsonlite)
  library(httr)
  library(readxl)
  library(siebanxicor)
  library(pdftools)
  library(rvest)
  library(IMFData)
})
options(warn=-1)

helper_directory <- "helper/"
output_directory <- "output/"
start_date <- as.Date("2002-01-01")
end_date <- as.Date("2020-04-01")


### get new data
source("src/get_data.r")
get_api_directory <- "src/get_api/"
get_data(start_date, end_date, helper_directory, output_directory, get_api_directory, "All")


options(warn=0)