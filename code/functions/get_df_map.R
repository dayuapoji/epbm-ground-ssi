get_point_data <- function(data, ID_list, ID_num) {
  # function to get data of an instrument point based on the requested ID
  # INPUT
  #   data    = data of an instrument, e.g., nssp, arsp
  #   ID_list = see function get_df_map
  #   ID_num  = row index of ID_list

  point_data <- data %>% 
    # filter data based on ID number
    filter(str_detect(ID, toString(ID_list[ID_num, ]))) %>% .[ ,2:3] %>%
    # set ID as column name
    set_colnames(c('Date', toString(ID_list[ID_num, ])))

  return(point_data)
}

get_df_map <- function(data, coord) {
  # function to get df of an instrument, transpose, 
  # and merge to the coordinates for map plotting
  # INPUT
  #   data  = data of an instrument, e.g., nssp, arsp
  #   coord = coordinates of instruments, e.g., coord_nssp, coord_arsp
  
  # get list ID of an instrument
  ID_list <- unique(data[1])
  
  # initialize df
  df <- get_point_data(data, ID_list, 1)
  # complete the df
  for (ID_num in 2:nrow(ID_list)) {
    # get data of an instrument
    point_data <- get_point_data(data, ID_list, ID_num)
    # bind data into df
    df <- merge(df, point_data, by = "Date", all = T)
  }
  
  # transpose df
  df_t <- t(df)
  # modify date and set as colnames
  df_t[1,] <- paste0("D", df_t[1,])
  colnames(df_t) <- df_t[1, ]
  df_t <- df_t[-c(1), ] %>% data.frame(.) %>% rownames_to_column(., var = "ID")
  
  # merge df with coordinates
  df_map <- merge(df_t, coord, by = "ID") %>% relocate(c('Long', 'Lat'))
  # convert data from char to numeric
  df_map[ , 4:ncol(df_map)] <- df_map[ , 4:ncol(df_map)] %>% 
    mutate_if(is.character, as.numeric)
  
  return(df_map)
}
