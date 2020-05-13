context("get_data")

library(tidyverse)
library(hash)
library(rsdmx)
library(jsonlite)
library(IMFData)

library(testthat)
source("../src/get_api.r")

# initializing data
helper_directory <- "../helper/"
start_date <- as.Date("2002-01-01")
end_date <- as.Date("2020-04-01")
start_year <- "2002"
start_quarter <- "2002-Q1"
end_quarter <- "2020-Q1"
cat <- read_csv(paste0(helper_directory,"catalog.csv"))
countries <- read_csv(paste0(helper_directory, "country_codes.csv"))

# function to generate the data necessary for an  API call
gen_data <- function (source_in, frequency_in) {
  cat_data <- cat %>% filter(source==source_in, frequency==frequency_in) %>% slice(1)
  data_hash <- hash()
  data_hash[["url"]] <- cat_data %>% select(url) %>% pull
  data_hash[["which_time"]] <- cat_data %>% select(frequency) %>% pull
  data_hash[["data_source"]] <- cat_data %>% select(source) %>% pull
  data_hash[["g"]] <- cat_data %>% select(download_group) %>% pull
  if (data_hash[["data_source"]] == "oecd") {
    start_url <- start_date
  } else if (data_hash[["data_source"]] == "eurostat") {
    start_url <- start_year
  } else {
    start_url <- ""
  }
  data_hash[["url"]] <- gen_url(data_hash[["url"]], start_url)
  if (source_in == "nbs") { data_hash[["url"]] <- str_replace(data_hash[["url"]], "START_YEAR", start_year) }
  return(data_hash)
}

# testing csvs read in correctly
test_that("cat file loaded correctly", {
  expect_gt(nrow(cat), 10)
})

test_that("countries file loaded correctly", {
  expect_gt(nrow(countries), 10)
})

# testing monthly oecd
data_hash <- gen_data("oecd", "m")
data <- get_api(data_hash[["url"]], cat, data_hash[["g"]], countries, data_hash[["which_time"]], data_hash[["data_source"]], start_date, end_date)
test_that("oecd monthly works", {
  expect_gt(nrow(data), 10)
})

# test quarterly oecd
data_hash <- gen_data("oecd", "q")
data <- get_api(data_hash[["url"]], cat, data_hash[["g"]], countries, data_hash[["which_time"]], data_hash[["data_source"]], start_date, end_date)
test_that("oecd quarterly works", {
  expect_gt(nrow(data), 10)
})

# test eurostat monthly
data_hash <- gen_data("eurostat", "m")
data <- get_api(data_hash[["url"]], cat, data_hash[["g"]], countries, data_hash[["which_time"]], data_hash[["data_source"]], start_date, end_date)
test_that("eurostat monthly works", {
  expect_gt(nrow(data), 10)
})

# test eurostat quarterly
data_hash <- gen_data("eurostat", "q")
data <- get_api(data_hash[["url"]], cat, data_hash[["g"]], countries, data_hash[["which_time"]], data_hash[["data_source"]], start_date, end_date)
test_that("eurostat quarterly works", {
  expect_gt(nrow(data), 10)
})

# test fred
data_hash <- gen_data("fred", "m")
data <- get_fred(data_hash[["url"]], cat, data_hash[["g"]], start_date, end_date)
test_that("fred works", {
  expect_gt(nrow(data), 10)
})

# test imf
data_hash <- gen_data("imf", "q")
data <- get_api(data_hash[["url"]], cat, data_hash[["g"]], countries, data_hash[["which_time"]], data_hash[["data_source"]], start_date, end_date)
test_that("imf works", {
 expect_gt(nrow(data), 10)
})

# test nbs single
data_hash <- gen_data("nbs", "m")
data <- get_nbs(data_hash[["url"]], cat, data_hash[["g"]], start_date, end_date)
test_that("nbs single works", {
 expect_gt(nrow(data), 10)
})

# test nbs double
url1 <- 'http://data.stats.gov.cn/english/easyquery.htm?m=QueryData&dbcode=hgyd&rowcode=zb&colcode=sj&wds=[]&dfwds=[{"wdcode":"sj","valuecode":"2002-2030"},{"wdcode":"zb","valuecode":"A0906"}]-FILTER-A09060A'
url2 <- 'http://data.stats.gov.cn/english/easyquery.htm?m=QueryData&dbcode=hgyd&rowcode=zb&colcode=sj&wds=[]&dfwds=[{"wdcode":"sj","valuecode":"2002-2030"},{"wdcode":"zb","valuecode":"A0905"}]-FILTER-A090502'
g <- 22
data <- get_nbs_double(url1, url2, cat, g, start_date, end_date)
test_that("nbs double works (group 22 hard coded in test)", {
  expect_gt(nrow(data), 10)
})

# url <- data_hash[["url"]]
# g <- data_hash[["g"]]
# which_time <- data_hash[["which_time"]]
# data_source <- data_hash[["data_source"]]