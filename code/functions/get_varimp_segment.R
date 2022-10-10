get_varimp_segment <- function(df_merge, boundary_df, tbm_list) {
  
  varimp_df <- NULL
  for (i in 1:nrow(boundary_df)) {
    
    # define segments according to specified boundary
    df_segment <- df_merge %>%
      filter(., DistanceHead > boundary_df$SegmentStart[i]) %>%
      filter(., DistanceHead <= boundary_df$SegmentEnd[i]) %>%
      reset_disp(.)
    
    # compute feature importance
    varimp_rf <- get_varimp_rf(df_segment %>% 
                                 select(-c('ChainageHead')) %>% 
                                 select(-c('ChainagePoint')) %>% 
                                 select(-c('ID')) %>%
                                 select(-c('Long')) %>% 
                                 select(-c('Lat')) %>% 
                                 select(-c('Value'))) %>%
      # segment label
      mutate(Segment = paste0('Segment-', i))
    
    varimp_df <- rbind(varimp_df, varimp_rf)
  }
  
  # set group for features
  varimp_df <- left_join(varimp_df, tbm_list$group, by = 'Feature') %>% 
    mutate(Group = replace_na(as.character(Group), 'Spatial Geometries'))
  
  return(varimp_df)
}
