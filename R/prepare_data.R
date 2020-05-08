prepare_data <- function(data, start, end) {
  ###
  ### Function to prepare final databases for use in nowcasting program in Octave
  ### Current models used for variable selection
  ###
  ### Inputs:
  ###    - data: current update of the database, seasonally adjusted and transformed as required (required, no default)
  ###    - start: starting data for estimation purposes
  ###    - end: ending date for estimation purposes
  ###
  ### Outputs:
  ###    - data_nowcast_goods_value: database for nowcast model of global merchandise trade in values
  ###    - data_nowcast_goods_volume: database for nowcast model of global merchandise trade in volumes
  ###    - data_nowcast_services: database for nowcast model of global trade in services
  ###
  ### Note: this function is only used for the current configuration of the model. Once the new one is implemented,
  ###       it won't be necessary since it will be implemented directly in Octave
  ###
  ### This version: Fernando Cantu, 2020-04-28
  ###
  
  ### Global merchandise trade in value
  vars <- c("date", "x_world.sa", "x_cn.sa", "x_us.sa", "x_kr.sa", "x_hk.sa", "ipi_us.sa", "ipi_jp.sa", "ipi_kr.sa",
            "p_comm.sa", "p_manuf.sa",	"x_uv_world.sa",	"x_vol_world.sa",	"freight_mar_cn.sa",
            "container_index.sa", "container_sg.sa", "bci_cn.sa",	"bci_oecd.sa",	"cci_cn.sa",
            "manuf_orders_nl.sa",	"pmi_manuf_us.sa",	"pmi_manuf_cn.sa")
  db <- data %>%
    select(vars) %>%
    mutate(date = as.Date(date)) %>%
    filter(date >= start)
  if(db[nrow(db), "date"] < end) {
    db <- rbind(db, matrix(NA, ncol = ncol(db), 
                           nrow = length(seq.Date(from = db[nrow(db), "date"], to = end, by = "month")) - 1, 
                           dimnames = list(NULL, names(db))))
    db$date <- seq.Date(from = start, to = end, by = "month")
  }
  db <- db %>%
    replace(is.na(.), -99999)
  data_nowcast_goods_value <- db
  
  ### Global merchandise trade in volume
  vars <- c("date", "x_vol_world2.sa", "x_vol_world.sa", "x_vol_ez.sa", "x_vol_ema.sa", "x_vol_lac.sa",
            "ipi_jp.sa", "ipi_de.sa", "ipi_kr.sa", "ipi_oecd.sa", "ind_orders_ez.sa",
            "manuf_orders_us_2.sa", "rti_vol_fr.sa", "rti_vol_oecd.sa", "freight_mar_cn.sa",
            "container_hk.sa", "transit_pa_canal.sa", "container_index.sa", "x_cn.sa",
            "x_oecd.sa",	"x_briics.sa", "bci_cn.sa", "pmi_manuf_us.sa", "pmi_manuf_cn.sa")
  db <- data %>%
    select(vars) %>%
    mutate(date = as.Date(date)) %>%
    filter(date >= start)
  if(db[nrow(db), "date"] < end) {
    db <- rbind(db, matrix(NA, ncol = ncol(db), 
                           nrow = length(seq.Date(from = db[nrow(db), "date"], to = end, by = "month")) - 1, 
                           dimnames = list(NULL, names(db))))
    db$date <- seq.Date(from = start, to = end, by = "month")
  }
  db <- db %>%
    replace(is.na(.), -99999)
  data_nowcast_goods_volume <- db
  
  ### Global trade in services
  vars <- c("date", "x_servs_world.sa",	"x_servs_oecd.sa", "x_servs_us.sa", "x_servs_uk2.sa",	"is_jp.sa",
            "x_uv_world.sa",	"x_vol_world.sa", "x_cn.sa", "x_oecd.sa", "ipi_oecd.sa",	"rti_val_us.sa",
            "freight_mar_cn.sa", "container_index.sa", "container_total_la.sa",	"container_sg.sa",	
            "tourists_world", "tourists_es.sa", "tourists_it.sa", "bci_servs_eu.sa",
            "pmi_nm_us.sa",	"pmi_nm_cn2.sa", "rti_cn")
  db <- data %>%
    select(vars) %>%
    mutate(date = as.Date(date)) %>%
    filter(date >= start)
  if(db[nrow(db), "date"] < end) {
    db <- rbind(db, matrix(NA, ncol = ncol(db), 
                           nrow = length(seq.Date(from = db[nrow(db), "date"], to = end, by = "month")) - 1, 
                           dimnames = list(NULL, names(db))))
    db$date <- seq.Date(from = start, to = end, by = "month")
  }
  db <- db %>%
    replace(is.na(.), -99999)
  data_nowcast_services <- db
  
  return(list(data_nowcast_goods_value = data_nowcast_goods_value,
              data_nowcast_goods_volume = data_nowcast_goods_volume,
              data_nowcast_services = data_nowcast_services))
  
}