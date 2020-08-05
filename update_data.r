suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(hash)
  library(rsdmx)
  library(jsonlite)
  library(httr)
  library(readxl)
  library(stringr)
  library(RCurl)
  library(siebanxicor)
  library(pdftools)
  library(rvest)
  library(IMFData)
  library(seasonal)
})
options(warn=-1)

args <- commandArgs(trailingOnly=TRUE)
helper_directory <- "helper/"
output_directory <- "output/"
start_date <- as.Date("2002-01-01")
run_date <- Sys.Date()
end_date <- as.Date(paste0(substr(run_date, 1, 4), "-", substr(run_date, 6, 7), "-01"))


### get new data
print("Getting new data...")
source("src/get_data.r")
get_api_directory <- "src/get_api/"
if (length(args) == 0) {
  groups <- "All" # 1:4 etc. for a different group 
} else {
  groups <- eval(parse(text=args[1]))
}
suppressMessages(
    get_data(start_date, end_date, run_date, helper_directory, output_directory, get_api_directory, groups)
)


### transform data (seasonality etc.)
print("Transforming data...")
source("src/transform_data.r")
suppressMessages(
  transform_data(run_date, output_directory)
)
cat("\033[0;32mTransformation successful\033[0m\n")

cat("\033[0;32mUpdate successful\033[0m\n")
options(warn=0)