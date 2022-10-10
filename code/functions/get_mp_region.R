get_mp_region <- function(mp_data, chainage, distance) {
  
  mp_region <- mp_data %>% 
    .[.$ChainagePoint >= chainage & .$ChainagePoint < (chainage+distance), ]
  
  return(mp_region)
}