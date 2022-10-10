get_list_ts <- function(data) {
  # function to get list to construct time series of each instrument
  # INPUT
  #   data = data of an instrument, e.g., nssp, arsp
  
  # initialize list
  list_ts <- list()
  # filling the list
  for(i in 18:28) {
    # create time series, filter according to the chainage, i.e., 18xxx to 28xxx 
    ts <- data %>% filter(str_detect(ID, paste0('_',i)))
    # include into the list
    list_ts[[i]] <- ts
  }
  
  return(list_ts)
}