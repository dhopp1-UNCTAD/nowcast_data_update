get_ibge <- function (url, catalog, g, start_date, end_date) {
  # services info: , https://sidra.ibge.gov.br/pesquisa/pms/tabelas
  # ipi info: https://sidra.ibge.gov.br/pesquisa/pim-pf-brasil/tabelas
  vars <- gen_vars(catalog, g)
  tmp <- gen_tmp(vars, start_date, end_date)
  
  months <- seq(as.Date(start_date), as.Date(end_date), by = "month") %>%
    sapply(function (x) paste0(substr(x, 1, 4), substr(x, 6, 7))) %>%
    unname() %>%
    paste(collapse="|")
  url <- str_replace(url,"MONTHS", months)
  
  status <- tryCatch({
    rawdata <- GET(url) %>% 
      .$content %>% 
      rawToChar() %>% 
      fromJSON()
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data <- rawdata %>% 
      slice(2:n()) %>% 
      mutate(date = as.Date(paste0(substr(D3C, 1, 4), "-", substr(D3C, 5, 6), "-01"))) %>% 
      select(date, V) %>% 
      rename(!!vars$code[1] := V)
    
    tmp <- tmp %>%
      select(date) %>%
      left_join(data, by="date")
    
    return(tmp %>% select(-1))
  }
}