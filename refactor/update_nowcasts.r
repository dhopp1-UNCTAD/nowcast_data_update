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
  library(seasonal)
})
options(warn=-1)

helper_directory <- "helper/"
output_directory <- "output/"
start_date <- as.Date("2002-01-01")
end_date <- as.Date("2020-04-01")


### get new data
print("Getting new data...")
source("src/get_data.r")
get_api_directory <- "src/get_api/"
get_data(start_date, end_date, helper_directory, output_directory, get_api_directory, "All")


### compare to old data (revisions, etc.)
print("Comparing to prior data...")
source("src/compare_data.r")
compare_data(end_date, output_directory, sensitivity=0.02)

### transform data (seasonality etc.)
print("Transforming data...")
source("src/transform_data.r")
transform_data(end_date, output_directory)

options(warn=0)