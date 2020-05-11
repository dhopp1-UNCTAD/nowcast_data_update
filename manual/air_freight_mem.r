library(tidyverse)
library(stringr)
library(pdftools)

year <- 2020
col_start <- 4 # which colums/element number each of these numbers appears in, want domestic/intl. deplaned and enplaned monthly total

# from here: https://www.flymemphis.com/statistics, calendar year monthly. URL have to be changed for 2021
url <- "https://www.flymemphis.com/Areas/Admin/Images/Upload_2020055092924.pdf"
pages_per_month <- 5 # how many pages each month gets
data_page <- 3 # on which page of each month the necessary data appears
all_data <- data.frame(year=2000, month=1, air_freight_mem=0)

pdf <- pdf_text(url)

for (i in 1:(length(pdf) / pages_per_month)) {
  start_page <- pages_per_month * i - pages_per_month + 1
  end_page <- start_page + pages_per_month - 1
  subset <- pdf[start_page:end_page]
  text <- subset[data_page]
  
  start_data <- str_locate_all(pattern="MONTHLY TOTALS", text)[[1]][2]
  data <- text %>% 
    substr(start_data, start_data + 1000) %>%
    str_replace_all("[\n| ]", "-") %>%
    str_replace_all(",", "")
    
  data <- gsub("([-])\\1{1,}", "\\1", data) %>% strsplit("-")
  dom_en <- data[[1]][col_start] %>% as.numeric
  dom_de <- data[[1]][col_start+1] %>% as.numeric
  intl_en <- data[[1]][col_start+2] %>% as.numeric
  intl_de <- data[[1]][col_start+3] %>% as.numeric
  
  tmp <- data.frame(year=year, month=i, air_freight_mem=dom_en+dom_de+intl_en+intl_de)
  all_data <- all_data %>%
    rbind(tmp)
}

all_data <- all_data %>% slice(2:n())