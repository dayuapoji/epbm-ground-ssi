get_mp_plot <- function(mp_data, start_date, end_date) {
  
  df_plot <- mp_data %>% drop_na(.) %>% 
    # convert date into posix format
    mutate(Date = as.POSIXct(Date, format = '%Y%m%d')) %>%
    # remove date outside the specified range
    .[.$Date >= start_date & .$Date <= end_date, ] %>%
    # order date from start data to end date
    .[order(.$Date, decreasing = F), ]%>% 
    # group by ID, 
    group_by(ID) %>%
    # reset displacement at start date to zero
    mutate(DeltaValue = Value - Value[1])
  
  return(df_plot)
}
