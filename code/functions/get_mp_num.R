get_mp_num <- function(mp_data, chainage, distance, dist_to_center) {
  mp_num <- mp_data %>%
    # select MPs with distance from center TDA <= the specified distance
    filter(., CenterDistance <= dist_to_center) %>%
    # get mp inside the specified region
    get_mp_region(., chainage, distance) %>%
    # get unique MPs
    distinct(ID) %>% nrow(.)
  
  return(mp_num)
}