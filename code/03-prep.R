# PREPARE DATA #################################################################
 

# TBM Coordinates --------------------------------------------------------------

# prep TBM positions
tbm_pos <- get_tbm_pos(tbm_orig)

# prep TBM data
tbm_list <- select_tbm_data(tbm_orig)
tbm_data <- tbm_list$data %>% 
  rename(ChainageHead = Chainage)

# prep tunnel depth
tunnel_depth <- tunnel_depth %>% 
  # remove anomaly value
  .[.$DepthTunnel > 5, ] 

# prep TBM head chainage
chainage_head <- tbm_pos %>% 
  # remove duplicated dates, select the last
  group_by(Date) %>% slice_tail() %>% 
  # select only date and chainage columns
  .[ , 1:2] %>% 
  rename(ChainageHead = Chainage) %>%
  # format date
  mutate(Date = strftime(Date, format = '%Y%m%d')) %>% 
  # remove chainage outside tunnel depth info range
  filter(ChainageHead > min(tunnel_depth$Chainage) & 
           ChainageHead < max(tunnel_depth$Chainage)) %>%
  # convert df to acceptable interp function
  data.frame() %>%
  # add tunnel depth column by interpolation
  mutate(DepthTunnel = approx(tunnel_depth$Chainage,
                              tunnel_depth$DepthTunnel,
                              .$ChainageHead)$y)

# MP coordinates ---------------------------------------------------------------

# get coord for all MPs
coord <- coord_orig %>%
  # select only relevant columns
  select(c('MP_NAME', 'Easting', 'Northing')) %>%
  # rename cols
  set_colnames(c('ID', 'Long', 'Lat'))

# get coord for each MP
mpbx_coord <- get_mp_coord(coord, '7')


# MP locations -----------------------------------------------------------------

# get mp to tunnel alignment distance
mpbx_loc <- get_mp_loc(mpbx_coord, tbm_pos) 

# set mp to tunnel crown distance
mpbx_01 <- mpbx_loc %>% 
  filter(., substr(ID, 16, 17) == "01") %>%
  # anchor 01 ~5 ft above tunnel crown, in meter
  mutate(DistanceCrown = 5 * 0.3048) %>%
  # mutate(DepthPoint = ifelse(is.na(DepthPoint), (DepthTunnel - (5 * 0.3048)), DepthPoint))
  drop_na()

mpbx_02 <- mpbx_loc %>% 
  filter(., substr(ID, 16, 17) == "02") %>%
  # anchor 02 ~10 ft above tunnel crown
  mutate(DistanceCrown = 10 * 0.3048) %>%
  # mutate(DepthPoint = ifelse(is.na(DepthPoint), (DepthTunnel - (10 * 0.3048)), DepthPoint))
  drop_na()

# mpbx_03 <- mpbx_loc %>%
#   filter(., substr(ID, 16, 17) == "03") %>%
#   # anchor 03 ~20 ft below ground surface
#   mutate(DistanceCrown = (approx(tunnel_depth$Chainage,
#                                  tunnel_depth$DepthTunnel,
#                                  .$ChainagePoint)$y) - (20 * 0.3048)) %>%
#   # mutate(DepthPoint = ifelse(is.na(DepthPoint), (20 * 0.3048), DepthPoint))
#   drop_na()
# 
# mpbx_04 <- mpbx_loc %>% 
#   filter(., substr(ID, 16, 17) == "04") %>%
#   # anchor 04 ~?80 ft above tunnel crown
#   mutate(DistanceCrown = 80 * 0.3048) %>%
#   # mutate(DepthPoint = ifelse(is.na(DepthPoint), (DepthTunnel - (80 * 0.3048)), DepthPoint)) %>%
#   # remove above SR
#   # filter(., DepthPoint > (20 * 0.3048))
# 
# mpbx_SR <- mpbx_loc %>%
#   filter(., substr(ID, 16, 17) == "SR") %>%
#   # anchor SR ~20 ft below ground surface
#   mutate(DistanceCrown = (approx(tunnel_depth$Chainage,
#                                  tunnel_depth$DepthTunnel,
#                                  .$ChainagePoint)$y)) %>%
#   # mutate(DepthPoint = ifelse(is.na(DepthPoint), (20 * 0.3048), DepthPoint))
#   drop_na()

# MP data -----------------------------------------------------------------

# MPBX
mpbx_data <- mpbx_orig %>% 
  # prep orig data
  prep_mp_data(.) %>%
  # connect to TBM head position
  merge(x = ., y = rbind(mpbx_01, mpbx_02), # mpbx_03, mpbx_SR), 
        by = 'ID', all.y = T) #%>%

# # setup parallelization
# num_cores <-  detectCores() - 1
# cl <- makeCluster(detectCores() - 1)
# registerDoParallel(cl, num_cores)
# system.time(
#   mpbx_loc <- get_mp_loc_par(mpbx_coord, tbm_pos)
# )
# stopCluster(cl)


# Merge MP & TBM Data ----------------------------------------------------------

df_merge <- mpbx_data %>%
  # merge mp data to chainage head by date
  merge(x = ., y = chainage_head, by = 'Date', all = F) %>%
  # get mp distance to TBM head, convert ft to m
  mutate(DistanceHead = (ChainageHead - ChainagePoint) * 0.3048) %>%
  # merge mp data to tbm data by chainage head
  merge(., tbm_data, by = 'ChainageHead', all = F, sort = F) %>%
  # remove unused columns
  # select(!contains('ChainageHead')) %>%
  # select(!contains('ChainagePoint')) %>%
  select(!contains('Date')) %>%
  select(!contains('Ring'))

# check
ggplot(df_merge %>%
         filter(., DistanceHead > -50) %>%
         filter(., DistanceHead <= 100) %>%
         reset_disp(.)) +
  geom_line(aes(x = DistanceHead, y = DeltaValue, color = ID)) +
  geom_smooth(aes(x = DistanceHead, y = DeltaValue), method = 'loess', color = 'black') +
  theme_bw() + 
  theme(legend.position = 'none')

# remove anomaly records
df_merge <- df_merge %>%
  # settlement/heaving unrealistically high?
  .[!(.$ID %in% c(df_merge %>%
                    filter(., DistanceHead > -50) %>%
                    filter(., DistanceHead <= 100) %>%
                    reset_disp(.) %>%
                    filter(., DeltaValue < -30) %>% .$ID %>% unique())), ] %>%
  .[!(.$ID %in% c(df_merge %>%
                    filter(., DistanceHead > -50) %>%
                    filter(., DistanceHead <= 100) %>%
                    reset_disp(.) %>%
                    filter(., DeltaValue > 10) %>% .$ID %>% unique())), ] %>%
  # settlemet/heaving far before tbm approaching?
  .[!(.$ID %in% c(df_merge %>%
                    filter(., DistanceHead > -50) %>%
                    filter(., DistanceHead <= -10) %>%
                    reset_disp(.) %>%
                    filter(., DeltaValue < -5) %>% .$ID %>% unique())), ] %>%
  .[!(.$ID %in% c(df_merge %>%
                    filter(., DistanceHead > -50) %>%
                    filter(., DistanceHead <= -10) %>%
                    reset_disp(.) %>%
                    filter(., DeltaValue > 2) %>% .$ID %>% unique())), ] 

# check
ggplot(df_merge %>%
         filter(., DistanceHead > -50) %>%
         filter(., DistanceHead <= 100) %>%
         reset_disp(.)) +
  geom_line(aes(x = DistanceHead, y = DeltaValue, color = ID)) +
  geom_smooth(aes(x = DistanceHead, y = DeltaValue), method = 'loess', color = 'black') +
  theme_bw() + 
  theme(legend.position = 'none')


# Save Results -----------------------------------------------------------------

# write_csv(mpbx_loc, '../results/mpbx_loc.csv')
# write_csv(mpbx_data, '../results/mpbx_data.csv')

