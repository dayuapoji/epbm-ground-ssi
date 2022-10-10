get_tbm_pos <- function(tbm_orig, 
                        scale_factor = 0.999987773,
                        E_offset = 699991.44,  
                        N_offset = 99998.78) {
  
  date <- tbm_orig %>%
    select(contains('record date')) %>%
    set_colnames('Date') %>% 
    mutate(Date = as.POSIXct(Date, format = "%m/%d/%Y")) %>%
    .[-c(1), ] 
  
  chainage <- tbm_orig %>%
    select(contains('chainage')) %>% 
    select(contains('head')) %>% 
    # select unit in ft
    .[ , 2] %>%
    set_colnames('Chainage') %>%
    set_df(.)
  
  tbm_crd <- tbm_orig %>%
    select(contains('crd')) %>%
    select(contains('head')) %>%
    set_colnames(c('N', 'E', 'Z')) %>%
    set_df(.)
  
  tbm_crd_deg <- tbm_crd[ , 1:2] %>% 
    # convert project coordinates (m) to State Plan projection (ft)
    mutate(E = `E`  * 3937 / 1200 / scale_factor + E_offset, 
           N = `N` * 3937 / 1200 / scale_factor + N_offset) %>%
    # convert state plane in US survey ft to longitude and latitude in deg  
    st_as_sf(coords = c ("E", "N")) %>% 
    st_set_crs(2285) %>% 
    st_transform(4326) %>% 
    mutate(Long = unlist(map(geometry,1)),
           Lat = unlist(map(geometry,2))) %>% 
    st_drop_geometry()
  
  tbm_pos <- cbind(date, chainage, tbm_crd_deg)
  
  return(tbm_pos)
}