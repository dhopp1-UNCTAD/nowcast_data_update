library(tidyverse)
library(jsonlite)
library(RCurl)
library(seasonal)

months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

get_target <- function (url) {
  rawdata <- fromJSON(getURL(url, encoding = "latin1"))
  data <- rawdata$Dataset %>% as_tibble
  data <- data %>%
    group_by(Year, Period) %>%
    summarise(value = sum(Value, na.rm=T)) %>%
    ungroup() %>%
    mutate(date = as.Date(paste0(Year, "-", sapply(Period, function (x) which(months == x)), "-01"))) %>%
    select(date, value)
  data_ts <- ts(data$value, frequency = 12, start = c(as.numeric(substr(min(me$date), 1, 4)), as.numeric(substr(min(me$date), 6, 7))))
  data_sa <- seas(window(data_ts, start = c(as.numeric(substr(min(me$date), 1, 4)), as.numeric(substr(min(me$date), 6, 7))), end = c(as.numeric(substr(max(me$date), 1, 4)), as.numeric(substr(max(me$date), 6, 7))), na.action = na.x13))
  data$sa <- data_sa$series$s11
  return(data %>% arrange(date))
}

# total merchandise exports, monthly (TO total merchandise)
me_url <- "https://api.wto.org/timeseries/v1/data?i=ITS_MTV_MX&r=all&p=000&pc=TO&spc=false&ps=all&max=1000000&fmt=json&mode=full&lang=1&meta=false&subscription-key=afc645dbdc7e433396cc37a022cb1fcd"
# total merchandise imports, monthly (TO total merchandise)
mi_url <- "https://api.wto.org/timeseries/v1/data?i=ITS_MTV_MM&r=all&p=000&pc=TO&spc=false&max=100000&fmt=json&mode=full&lang=1&meta=false&subscription-key=afc645dbdc7e433396cc37a022cb1fcd"
# Commercial service exports of selected economies, monthly (SOX commercial services)
se_url <- "https://api.wto.org/timeseries/v1/data?i=ITS_CS_MX&r=all&p=000&pc=SOX&spc=false&max=100000&fmt=json&mode=full&lang=1&meta=false&subscription-key=afc645dbdc7e433396cc37a022cb1fcd"
# Commercial service imports of selected economies, monthly (SOX commercial services)
si_url <- "https://api.wto.org/timeseries/v1/data?i=ITS_CS_MM&r=all&p=000&pc=SOX&spc=false&max=100000&fmt=json&mode=full&lang=1&meta=false&subscription-key=afc645dbdc7e433396cc37a022cb1fcd"

me <- get_target(me_url)
mi <- get_target(mi_url)
se <- get_target(se_url)
si <- get_target(si_url)

date_till <- min(c(max(me$date), max(mi$date), max(se$date), max(si$date)))

final_data <- me %>% filter(date <= date_till) %>% select(date, value) %>% rename(me=value) %>%
  left_join(mi %>% filter(date <= date_till) %>% select(date, value) %>% rename(mi=value), on="date") %>%
  left_join(se %>% filter(date <= date_till) %>% select(date, value) %>% rename(se=value), on="date") %>%
  left_join(si %>% filter(date <= date_till) %>% select(date, value) %>% rename(si=value), on="date")

write_csv(final_data, "targets.csv")