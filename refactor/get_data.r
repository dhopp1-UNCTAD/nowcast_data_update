library(tidyverse)
library(hash)

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
### Getting monthly OECD data
# function to get a download group g and column bind to the database
get_oecd_monthly <- function (url, cat, g, countries) {
  vars <- cat %>% filter(download_group == g)
  tmp <- as.data.frame(
    matrix(NA, 
           ncol = (nrow(vars) + 1), 
           nrow = length(seq(from = start_date, to = end_date, by = "month"))
    )
  ) %>%
    rename_at(vars(colnames(.)), ~c("date", vars$code)) %>%
    mutate(date = seq(from = start_date, to = end_date, by = "month"))
  
  status <- tryCatch({
    rawdata <- readSDMX(url)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data <- as.data.frame(rawdata) %>%
      left_join(countries, by = c("LOCATION" = "oecd"))
    for (i in 1:nrow(vars)) {
      datai <- data %>% filter(country == as.character(vars[i, "country"]))
      starti <- which(grepl(as.Date(paste0(datai[1, "obsTime"], "-01"), format = "%Y-%m-%d"), tmp$date))
      tmp[starti:(starti + nrow(datai) - 1), i + 1] <- datai$obsValue
    }
    # updating the log
    # log <<- log %>%
    #   mutate(status = ifelse(download_group == g, 0, download_group))
    log[log$download_group == g, "status"] <- 0
    
    return(tmp %>% select(-1))
  }
}
monthly_data <- hash()
gen_url <- function (url) {paste0(url,format(start_date, "%Y-%m"))}
# Group 1: Merchandise exports, source = OECD (monthly)
monthly_data[["1"]] <- "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/MEI/FRA+DEU+ITA+JPN+KOR+NLD+GBR+USA+OECD+CHN+IND+BRIICS.XTEXVA01.CXMLSA.M/all?startTime="
# Group 2: Industrial production indices, source = OECD (monthly)
monthly_data[["2"]] <- "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/MEI/FRA+DEU+ITA+JPN+KOR+MEX+GBR+USA+OECD.PRMNTO01.IXOBSA.M/all?startTime="
# Group 3: Retail trade indices in value, source = OECD (monthly)
monthly_data[["3"]] <- "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/MEI/CAN+FRA+DEU+ITA+JPN+GBR+USA.SLRTTO02.IXOBSA.M/all?startTime="
# Group 4: Retail trade indices in volume, source = OECD (monthly)
monthly_data[["4"]] <- "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/MEI/CAN+FRA+DEU+ITA+JPN+GBR+USA+OECD+BRA.SLRTTO01.IXOBSA.M/all?startTime="
# Group 5: Construction indices, source = OECD (monthly)
monthly_data[["5"]] <- "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/MEI/USA+OECD.PRCNTO01.IXOBSA.M/all?startTime="

for (g in 1:length(monthly_data)) {
  url <- gen_url(monthly_data[[as.character(g)]])
  database <- cbind(
    database,
    get_oecd_monthly(url, cat, g, countries)  
  )
  log[log$download_group == g, "status"] <- 0
}