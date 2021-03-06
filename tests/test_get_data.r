context("get_data")

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

library(testthat)
begin_path <- "../src/get_api/"
files <- list.files(begin_path)
files <- sapply(files, function(x) paste0(begin_path, x))
sapply(files, source)

# initializing data
helper_directory <- "../helper/"
start_date <- as.Date("2002-01-01")
end_date <- as.Date("2020-04-01")
catalog <- read_csv(paste0(helper_directory,"catalog.csv"))
countries <- read_csv(paste0(helper_directory, "country_codes.csv"))
historical <- read_csv("../helper/historical.csv")
eikon <- read_excel("../helper/Eikon.xlsx")

# function to generate the data necessary for an  API call
gen_data <- function (source_in, frequency_in) {
  cat_data <- catalog %>% filter(source==source_in, frequency==frequency_in) %>% slice(1)
  data_hash <- hash()
  data_hash[["url"]] <- cat_data %>% select(url) %>% pull
  data_hash[["which_time"]] <- cat_data %>% select(frequency) %>% pull
  data_hash[["data_source"]] <- cat_data %>% select(source) %>% pull
  data_hash[["g"]] <- cat_data %>% select(download_group) %>% pull
  return(data_hash)
}

###
### Testing loop set up

# testing csvs read in correctly
test_that("catalog file loaded correctly", {
  expect_gt(nrow(catalog), 10)
})

test_that("countries file loaded correctly", {
  expect_gt(nrow(countries), 10)
})

test_that("data hash generated correctly", {
  expect_gt(length(gen_data_hash(catalog)), 10)
})


###
### Testing api calls

# testing monthly oecd
data_hash <- gen_data("oecd", "m")
data <- get_api(data_hash[["url"]], catalog, data_hash[["g"]], countries, data_hash[["which_time"]], data_hash[["data_source"]], start_date, end_date)
test_that("oecd monthly works", {
  expect_equal(nrow(data), 220)
})

# test quarterly oecd
data_hash <- gen_data("oecd", "q")
data <- get_api(data_hash[["url"]], catalog, data_hash[["g"]], countries, data_hash[["which_time"]], data_hash[["data_source"]], start_date, end_date)
test_that("oecd quarterly works", {
  expect_equal(nrow(data), 220)
})

# test eurostat monthly
data_hash <- gen_data("eurostat", "m")
data <- get_api(data_hash[["url"]], catalog, data_hash[["g"]], countries, data_hash[["which_time"]], data_hash[["data_source"]], start_date, end_date)
test_that("eurostat monthly works", {
  expect_equal(nrow(data), 220)
})

# test eurostat quarterly
data_hash <- gen_data("eurostat", "q")
data <- get_api(data_hash[["url"]], catalog, data_hash[["g"]], countries, data_hash[["which_time"]], data_hash[["data_source"]], start_date, end_date)
test_that("eurostat quarterly works", {
  expect_equal(nrow(data), 220)
})

# test fred
data_hash <- gen_data("fred", "m")
data <- get_fred(data_hash[["url"]], catalog, data_hash[["g"]], start_date, end_date)
test_that("fred works", {
  expect_equal(nrow(data), 220)
})

# test imf
data_hash <- gen_data("imf", "q")
data <- get_api(data_hash[["url"]], catalog, data_hash[["g"]], countries, data_hash[["which_time"]], data_hash[["data_source"]], start_date, end_date)
test_that("imf works", {
  expect_equal(nrow(data), 220)
})

# test nbs single
data_hash <- gen_data("nbs", "m")
data <- get_nbs(data_hash[["url"]], catalog, data_hash[["g"]], start_date, end_date)
test_that("nbs single works", {
  expect_equal(nrow(data), 220)
})

# test nbs double
url1 <- 'http://data.stats.gov.cn/english/easyquery.htm?m=QueryData&dbcode=hgyd&rowcode=zb&colcode=sj&wds=[]&dfwds=[{"wdcode":"sj","valuecode":"START_YEAR-2030"},{"wdcode":"zb","valuecode":"A0906"}]-FILTER-A09060A'
url2 <- 'http://data.stats.gov.cn/english/easyquery.htm?m=QueryData&dbcode=hgyd&rowcode=zb&colcode=sj&wds=[]&dfwds=[{"wdcode":"sj","valuecode":"START_YEAR-2030"},{"wdcode":"zb","valuecode":"A0905"}]-FILTER-A090502'
g <- 22
data <- get_nbs_double(url1, url2, catalog, g, start_date, end_date)
test_that("nbs double works (group 22 hard coded in test)", {
  expect_equal(nrow(data), 220)
})

# test hk nso
data_hash <- gen_data("hk nso", "m")
data <- get_hk_nso(data_hash[["url"]], catalog, data_hash[["g"]], start_date, end_date, historical)
test_that("hk nso works", {
  expect_equal(nrow(data), 220)
})

# test boj
data_hash <- gen_data("boj", "m")
data <- get_boj(data_hash[["url"]], catalog, data_hash[["g"]], start_date, end_date)
test_that("boj works", {
  expect_equal(nrow(data), 220)
})

# test ecb
data_hash <- gen_data("ecb", "m")
data <- get_single_api(data_hash[["url"]], catalog, data_hash[["g"]], data_hash[["which_time"]], data_hash[["data_source"]], start_date, end_date, "obsTime", "obsValue")
test_that("ecb works", {
  expect_equal(nrow(data), 220)
})

# test wto monthly
data_hash <- gen_data("wto", "m")
data <- get_single_api(data_hash[["url"]], catalog, data_hash[["g"]], data_hash[["which_time"]], data_hash[["data_source"]], start_date, end_date, "date", "Dataset.Value")
test_that("wto monthly works", {
  expect_equal(nrow(data), 220)
})

# test wto quarterly
data_hash <- gen_data("wto", "q")
data <- get_single_api(data_hash[["url"]], catalog, data_hash[["g"]], data_hash[["which_time"]], data_hash[["data_source"]], start_date, end_date, "date", "Dataset.Value")
test_that("wto quarterly works", {
  expect_equal(nrow(data), 220)
})

# test cpb
data_hash <- gen_data("cpb", "m")
data <- get_cpb(data_hash[["url"]], catalog, data_hash[["g"]], countries, start_date, end_date)
test_that("cpb works", {
  expect_equal(nrow(data), 220)
})

# test banxico
data_hash <- gen_data("banxico", "m")
data <- get_banxico(data_hash[["url"]], catalog, data_hash[["g"]], start_date, end_date)
test_that("banxico works", {
  expect_equal(nrow(data), 220)
})

# test ec
data_hash <- gen_data("ec", "m")
data <- get_ec(data_hash[["url"]], catalog, data_hash[["g"]], start_date, end_date)
test_that("ec works", {
  expect_equal(nrow(data), 220)
})

# test rwi
data_hash <- gen_data("rwi/isl", "m")
data <- get_rwi(data_hash[["url"]], catalog, data_hash[["g"]], start_date, end_date)
test_that("rwi/isl works", {
  expect_equal(nrow(data), 220)
})

# test meti
data_hash <- gen_data("meti", "m")
data <- get_meti(data_hash[["url"]], catalog, data_hash[["g"]], start_date, end_date)
test_that("meti works", {
  expect_equal(nrow(data), 220)
})

# test ons
data_hash <- gen_data("ons", "m")
data <- get_ons(data_hash[["url"]], catalog, data_hash[["g"]], start_date, end_date)
test_that("ons works", {
  expect_equal(nrow(data), 220)
})

# test ibge
data_hash <- gen_data("ibge", "m")
data <- get_ibge(data_hash[["url"]], catalog, data_hash[["g"]], start_date, end_date)
test_that("ons works", {
  expect_equal(nrow(data), 220)
})

# test nso_tr
data_hash <- gen_data("nso_tr", "q")
data <- get_nso_tr(data_hash[["url"]], catalog, data_hash[["g"]], start_date, end_date)
test_that("nso_tr works", {
  expect_equal(nrow(data), 220)
})

# test nso_sg
data_hash <- gen_data("nso_sg", "m")
data <- get_nso_sg(data_hash[["url"]], catalog, data_hash[["g"]], start_date, end_date)
test_that("nso_sg works", {
  expect_equal(nrow(data), 220)
})

# test lhr
data_hash <- gen_data("lhr", "m")
data <- get_lhr(data_hash[["url"]], catalog, data_hash[["g"]], start_date, end_date)
test_that("lhr works", {
  expect_equal(nrow(data), 220)
})

# test hkg
data_hash <- gen_data("hkg", "m")
data <- get_hkg(data_hash[["url"]], catalog, data_hash[["g"]], start_date, end_date)
test_that("hkg works", {
  expect_equal(nrow(data), 220)
})

# test unctad monthly
data_hash <- gen_data("unctad", "m")
data <- get_unctad(data_hash[["url"]], catalog, data_hash[["g"]], data_hash[["which_time"]], start_date, end_date)
test_that("unctad monthly works", {
  expect_equal(nrow(data), 220)
})

# test unctad quarterly
data_hash <- gen_data("unctad", "q")
data <- get_unctad(data_hash[["url"]], catalog, data_hash[["g"]], data_hash[["which_time"]], start_date, end_date)
test_that("unctad quarterly works", {
  expect_equal(nrow(data), 220)
})

# test eikon
data_hash <- gen_data("eikon", "m")
data <- get_eikon(catalog, data_hash[["g"]], start_date, end_date, eikon)
test_that("eikon works", {
  expect_equal(nrow(data), 220)
})

# test hk_ports
data_hash <- gen_data("hk_ports", "m")
data <- get_hk_ports(data_hash[["url"]], catalog, data_hash[["g"]], start_date, end_date, historical)
test_that("hk ports works", {
  expect_equal(nrow(data), 220)
})

# test pa canal
data_hash <- gen_data("pa_canal", "m")
data <- get_pa_canal(data_hash[["url"]], catalog, data_hash[["g"]], start_date, end_date, historical)
test_that("pa_canal works", {
  expect_equal(nrow(data), 220)
})

# test suez canal
data_hash <- gen_data("suez_canal", "m")
data <- get_suez_canal(data_hash[["url"]], catalog, data_hash[["g"]], start_date, end_date, historical)
test_that("suez_canal works", {
  expect_equal(nrow(data), 220)
})

# test memphis airport
data_hash <- gen_data("mem", "m")
data <- get_mem(data_hash[["url"]], catalog, data_hash[["g"]], start_date, end_date, historical)
test_that("mem works", {
  expect_equal(nrow(data), 220)
})

# test la port
data_hash <- gen_data("la_port", "m")
data <- get_la_port(data_hash[["url"]], catalog, data_hash[["g"]], start_date, end_date, historical)
test_that("la_port works", {
  expect_equal(nrow(data), 220)
})

# test manual/historical ones
data_hash <- gen_data("unctad_services", "q")
data <- get_manual(catalog, data_hash[["g"]], start_date, end_date, historical)
test_that("manual/historical ones one column works", {
  expect_equal(nrow(data), 220)
})
data_hash <- gen_data("unwto", "m")
data <- get_manual(catalog, data_hash[["g"]], start_date, end_date, historical)
test_that("manual/historical ones multiple column works", {
  expect_equal(nrow(data), 220)
})