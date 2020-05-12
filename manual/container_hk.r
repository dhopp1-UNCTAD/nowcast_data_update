library(tidyverse)
library(httr)

# info on the API: https://data.gov.hk/en/help/api-spec#historicalAPI, https://data.gov.hk/en-data/dataset/hk-thb-thb-mpb/resource/a1c00d18-25b4-4f36-8724-a112fd1373e3
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
# want the "Total ( '000 TEUs)" column
total_teu_col_num <- 5

# to get a file from a specific time
# api_url <- "https://api.data.gov.hk/v1/historical-archive/get-file?url="
# file_name <- "https://www.hkmpb.gov.hk/document/HKP_KTCT-stat_csv1(EN).csv&time="
# as_of <- "20190516-0933"
# final_url <- paste(api_url, file_name, as_of, sep="")

# get latest file
url <- "https://www.hkmpb.gov.hk/document/HKP_KTCT-stat_csv1(EN).csv"
data <- GET(url) %>% content %>% as_tibble
data <- data %>%
  filter(month != "All") %>%
  select(c(1:2, total_teu_col_num)) %>%
  rename(container_hk=3, year=Year) %>%
  mutate(month = sapply(month, function(x) which(x == months)))
