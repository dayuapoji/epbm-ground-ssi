get_mp_loc <- function(mp_coord, tbm_pos) {
  
  "Function to get monitoring points (MP) along center of TBM trajectory
    
   Input
    mp_coord: df of mp coordinates, from get_mp_coord function,
    tbm_pos: df of tbm position, from get_tbm_pos function,
  
   Output: df"
  
  # compute distance from monitoring points to tunnel alignment
  d <- dist2Line(p = as.matrix(mp_coord[, c('Long', 'Lat')]),
                 line = as.matrix(tbm_pos[, c('Long', 'Lat')]))
  mp_coord$DistanceAlignment <- unname(d[,1])
  
  # for each monitoring points, find rep chainage on tunnel alignment
  mp_coord$Chainage <- NA
  for (i in 1:nrow(d)) {
    # find distance between tbm coordinates and 
    # the nearest mp coordinates projection in the tbm coordinates line
    dist_chain <- distm(tbm_pos[, c('Long', 'Lat')], d[i, c('lon', 'lat')])
    index <- which(dist_chain == min(dist_chain))
    index <- index[1] # select 1st index
    # note: should add the distance, 
    # but we dont know (+/-), and have to be converted m to ft
    mp_coord$Chainage[i] <- tbm_pos$Chainage[index] 
  }
  
  mp_loc <- mp_coord %>% rename(ChainagePoint = Chainage)
  
  return(mp_loc)
}

# parallel processes
get_mp_loc_par <- function(mp_coord, tbm_pos) {
  mp_center <- foreach(i=1:nrow(mp_coord), .combine = 'rbind') %dopar% {
    library(tidyverse)
    library(geosphere)
    source("../../functions/get_mp_loc.R")
    get_mp_loc(mp_coord[i, ], tbm_pos)
  }
}
