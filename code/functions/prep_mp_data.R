prep_mp_data <- function(data_orig) {
  
  data <- data_orig %>% 
    # remove unused columns
    select(-c('Site name', 'Unit', 'Status')) %>% 
    # modify ID
    rename(ID = `Measure point name`) %>% 
    # modify date time
    separate(., Date, into = c("Date", "Time", "Note"), sep = " ") %>%
    mutate(Date = as.POSIXct(Date, format = "%m/%d/%Y")) %>%
    mutate(Date = strftime(Date, format = '%Y%m%d')) %>%
    select(-c('Time', 'Note')) %>%
    # convert inch to mm
    mutate(Value = Value * 25.4)
  
  return(data)
}

