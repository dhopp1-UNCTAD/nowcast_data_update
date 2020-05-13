library(tidyverse)
library(hash)
library(rsdmx)
library(jsonlite)
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
# Group 1: Merchandise exports, source = OECD (monthly)
data_hash[["1"]] <- c("https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/MEI/FRA+DEU+ITA+JPN+KOR+NLD+GBR+USA+OECD+CHN+IND+BRIICS.XTEXVA01.CXMLSA.M/all?startTime=", "monthly", "oecd")
# Group 2: Industrial production indices, source = OECD (monthly)
data_hash[["2"]] <- c("https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/MEI/FRA+DEU+ITA+JPN+KOR+MEX+GBR+USA+OECD.PRMNTO01.IXOBSA.M/all?startTime=", "monthly", "oecd")
# Group 3: Retail trade indices in value, source = OECD (monthly)
data_hash[["3"]] <- c("https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/MEI/CAN+FRA+DEU+ITA+JPN+GBR+USA.SLRTTO02.IXOBSA.M/all?startTime=", "monthly", "oecd")
# Group 4: Retail trade indices in volume, source = OECD (monthly)
data_hash[["4"]] <- c("https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/MEI/CAN+FRA+DEU+ITA+JPN+GBR+USA+OECD+BRA.SLRTTO01.IXOBSA.M/all?startTime=", "monthly", "oecd")
# Group 5: Construction indices, source = OECD (monthly)
data_hash[["5"]] <- c("https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/MEI/USA+OECD.PRCNTO01.IXOBSA.M/all?startTime=", "monthly", "oecd")
# Group 6: Exports of services, source = OECD (quarterly)
data_hash[["6"]] <- c("https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/MEI_BOP6/B6CRSE01.OECD.CXCUSA.Q/all?startTime=", "quarterly", "oecd")
# Group 7: FDI inflows, source = OECD (quarterly)
data_hash[["7"]] <- c("https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/FDI_AGGR_SUMM/OECD+WLD.USD.DI.T_FA_F/all?startTime=", "quarterly", "oecd")
# Group 8: Consumer confidence indices, source = OECD (monthly)
data_hash[["8"]] <- c("https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/MEI/DEU+JPN+GBR+USA+OECD+CHN.CSCICP03.IXNSA.M/all?startTime=", "monthly", "oecd")
# Group 9: Business confidence indices, source = OECD (monthly)
data_hash[["9"]] <- c("https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/MEI/DEU+JPN+NLD+GBR+USA+OECD+CHN.BSCICP03.IXNSA.M/all?startTime=", "monthly", "oecd")
# Group 10: Order books, source = OECD (monthly)
data_hash[["10"]] <- c("https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/MEI/DEU+NLD+GBR+USA.BSOBLV02.STSA.M/all?startTime=", "monthly", "oecd")
# Group 11: Exports of services, source = Eurostat (monthly)
data_hash[["11"]] <- c("http://ec.europa.eu/eurostat/SDMX/diss-web/rest/data/bop_c6_m/.MIO_EUR.S.S1.S1.CRE.WRL_REST.DE+FR.?startPeriod=", "monthly", "eurostat")
# Group 12: Exports of services, source = Eurostat (quarterly)
data_hash[["12"]] <- c("http://ec.europa.eu/eurostat/SDMX/diss-web/rest/data/bop_c6_q/.MIO_EUR.S.S1.S1.CRE.WRL_REST.IE+NL+UK.?startPeriod=", "quarterly", "eurostat")
# Group 13: Maritime freight, source = Eurostat (quarterly)
data_hash[["13"]] <- c("http://ec.europa.eu/eurostat/SDMX/diss-web/rest/data/mar_go_qm/.TOTAL.TOTAL.THS_T.DE+NL?startPeriod=", "quarterly", "eurostat")
# Group 14: Industrial production index, source = Eurostat (monthly)
data_hash[["14"]] <- c("http://ec.europa.eu/eurostat/SDMX/diss-web/rest/data/sts_inpr_m/.PROD.C.SCA.I15.EU27_2020?startPeriod=", "monthly", "eurostat")
# Group 15: Services production index, source = Eurostat (monthly)
data_hash[["15"]] <- c("http://ec.europa.eu/eurostat/SDMX/diss-web/rest/data/sts_sepr_m/.PROD.G-N_STS.SCA.I15.FR?startPeriod=", "monthly", "eurostat")
# Group 16: Tourist arrivals, source = Eurostat (monthly)
data_hash[["16"]] <- c("http://ec.europa.eu/eurostat/SDMX/diss-web/rest/data/tour_occ_arm/.FOR.NR.I551-I553.DE+ES+FR+IT+UK?startPeriod=", "monthly", "eurostat")
# Group 17: Exports of services, source = FRED (monthly)
#data_hash[["17"]] <- c("https://api.stlouisfed.org/fred/series/observations?series_id=BOPSEXP&api_key=2de1403493a03c96c5af253fca05abd2&file_type=json", "monthly", "fred")


for (g in 1:length(data_hash)) {
  print(paste("Fetching group", g))
  
  url <-data_hash[[as.character(g)]][1]
  which_time <-data_hash[[as.character(g)]][2]
  data_source <- data_hash[[as.character(g)]][3]
      
  if (data_source == "oecd") {
    start_url <- start_date
  } else if (data_source == "eurostat") {
    start_url <- start_year
  } else {
    start_url <- ""
  }
  url <- gen_url(url, start_url)
  database <- cbind(
    database,
    get_api(url, cat, g, countries, which_time, data_source)
  )
  log[log$download_group == g, "status"] <- 0
}