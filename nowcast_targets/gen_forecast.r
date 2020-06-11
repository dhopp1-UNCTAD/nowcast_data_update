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
  library(nowcasting)
})

# initial parameters
end_date <- as.Date("2020-06-01")
start_date <- as.Date("2002-01-01")
db_directory <- "../output/"
helper_directory <- "../helper/"
add_month <- function (x) {if (x == 12) {return (1)} else {return (x + 1)}}
catalog <- read_csv(paste0(helper_directory, "catalog.csv"))

# reading in data
data <- read_csv(paste0(db_directory, end_date, "_database_tf.csv")) %>%
  select(-date) %>%
  slice(2:n()) %>%
  ts(start=c(as.numeric(substr(start_date, 1, 4)), substr(start_date, 6, 7) %>% as.numeric %>% add_month), frequency=12)

# gen forecast
gen_forecast <- function (data, target) {
  if (target == "x_world.sa") {
    target_col <- "octave_value"  
  } else if (target == "x_vol_world2.sa") {
    target_col <- "octave_volume"
  } else {
    target_col <- "octave_services"
  }
  targets <- c()
  
  xs <- catalog %>%
    filter(!is.na(!!sym(target_col))) %>%
    select(!!sym(target_col)) %>%
    unique %>%
    pull
  frequency <- catalog %>% 
    slice(lapply(xs, function (r) which(r==catalog[,target_col])[1]) %>% unlist) %>% 
    select(frequency) %>%
    pull
  frequency <- lapply(frequency, function (r) if (r == "m") {12} else {4}) %>% unlist
  blocks <- cbind(as.matrix(rep(1, length(frequency))), as.matrix(rep(1, length(frequency))))
  x <- data[,xs]
  
  x <- Bpanel(x, trans=rep(0, ncol(x)), na.prop=1, NA.replace=F)
  
  # forecast
  nowEM <- nowcast(formula = as.formula(paste0(target, "~ .")), data=x, r=2, p=2, method="EM", blocks=blocks, frequency=frequency)
  
  forecast <- nowEM$yfcst %>% as.tibble
  dates <- c()
  counter <- 1
  for (i in 2002:2021) {
    for (j in c("03-01", "06-01", "09-01", "12-01")) {
      dates[counter] <- as.Date(paste0(i, "-", j))
      counter <- counter + 1
    }
  }
  dates <- dates[1:nrow(forecast)]
  forecast$date <- dates %>% as.Date(origin="1970-01-01")
  return (forecast)
}

# forecasts
for (target in c("x_world.sa", "x_vol_world2.sa", "x_servs_world.sa")) {
  forecast <- gen_forecast(data, target)
  
  forecast %>%
    gather(series, value, -date) %>%
    ggplot() + 
    aes(x=date, y=value, color=series) +
    geom_line() +
    ggtitle(target)
  ggsave(paste0(target, ".jpg"), units = "cm", width = 20, height = 10)
  write_csv(forecast, paste0(target, "_data.csv"))
}