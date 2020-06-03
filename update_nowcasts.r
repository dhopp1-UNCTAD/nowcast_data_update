suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
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

args <- commandArgs(trailingOnly=TRUE)
helper_directory <- "helper/"
output_directory <- "output/"
start_date <- as.Date("2002-01-01")
end_date <- as.Date(args[1])


### get new data
print("Getting new data...")
source("src/get_data.r")
get_api_directory <- "src/get_api/"
if (length(args) == 1) {
  groups <- "All" # 1:4 etc. for a different group 
} else {
  groups <- eval(parse(text=args[2]))
}
suppressMessages(
  get_data(start_date, end_date, helper_directory, output_directory, get_api_directory, groups)
)


### compare to old data (revisions, etc.)
print("Comparing to prior data...")
source("src/compare_data.r")
suppressMessages(
  compare_data(end_date, output_directory, sensitivity=0.02)
)

### transform data (seasonality etc.)
print("Transforming data...")
source("src/transform_data.r")
suppressMessages(
  transform_data(end_date, output_directory)
)
cat("\033[0;32mTransformation successful\033[0m\n")

### prepare data for matlab
print("Finalizing/preparing data...")
source("src/prepare_data.r")
prepare_data(start_date, end_date, output_directory)
cat("\033[0;32mPreparation successful\033[0m\n")

cat("\033[0;32mUpdate successful\033[0m\n")
options(warn=0)