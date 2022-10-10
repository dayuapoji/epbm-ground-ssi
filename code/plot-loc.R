# InSAR
insar <- insar_orig %>% 
  # remove ID column
  .[,2:ncol(.)] %>% 
  # remove column with NA
  .[ , colSums(is.na(.)) == 0] %>%
  # start from zero (remove 1st & 2nd dates)
  .[ , c(1,2,5:ncol(.))]

colnames(insar)[1:2] <- c('Long', 'Lat')

insar <-insar %>%
  .[which(.$Long > -122.35), ] %>%
  .[which(.$Long < -122.33), ] %>%
  .[which(.$Lat > 47.595), ] %>%
  .[which(.$Lat < 47.625), ]

i <- 39

# get date for each column iteration
date_i <- names(insar)[i] %>% substr(., 2, nchar(.)) %>% 
  as.POSIXct(., format = "%Y%m%d")

# get TBM position before the date
tbm_pos_i <- tbm_pos[which(tbm_pos$Date < date_i), ]

fig_insar <- ggplot() +
  
  # plot insar
  geom_point(data = insar,
             mapping = aes_string(x = 'Long', y = 'Lat',
                                  color = names(insar)[i]),
             size = 0.5) +
  
  scale_color_gradientn(name = paste0("InSAR (mm)"),
                        colours =  c(heat.colors(5)[1:4], "grey", 
                                     rev(topo.colors(5))[2:5]),
                        # limits = c(-16, 16),
                        # breaks = c(-15, -10, -5, 0, 5, 10, 15)) +
                        limits = c(-12, 12),
                        breaks = c(-12, -9, -6, -3, 0, 3, 6, 9, 12)) +
                        # limits = c(-6, 6),
                        # breaks = c(-6, -4.5, -3, -1.5, 0, 1.5, 3, 4.5, 6)) +    
  
  # plot TBM
  geom_point(data = tbm_pos_i,
             mapping = aes(x = Long, y = Lat), size = 0.1) +
  
  # set axes
  xlab("Longitude") + 
  ylab ("Latitude") +
  xlim(min(insar$Long), max(insar$Long)) +
  ylim(min(insar$Lat), max(insar$Lat)) +
  
  coord_equal() +
  theme_bw(base_size = 18) + 
  theme(legend.position = c(0.75, 0.80),)
  # theme(panel.grid = element_blank())
  
  
# ==============================================================================
# PLOT MAP - ARSP
# ==============================================================================
# plot all data points
fig_mp <- ggplot() +
  # TBM route
  geom_point(data = tbm_pos,  aes(x = Long, y = Lat, color = 'Tunnel'),
             size = 0.5) +
  # NSSP
  geom_point(data = nssp_center, aes(x = Long, y = Lat, color = 'NSSP'),
             size = 1.5, alpha = 0.5) +
  # ARSP
  geom_point(data = arsp_center, aes(x = Long, y = Lat, color = 'ARSP'),
             size = 1.5, alpha = 0.5) +
  # MPBX
  geom_point(data = mpbx_center, aes(x = Long, y = Lat, color = 'MPBX'),
             size = 1.5, alpha = 0.5) +
  scale_color_manual(name = "Monitoring Points", 
                     values = c('Tunnel' = 'grey',
                                'NSSP' = 'green',
                                'ARSP' = 'blue',
                                'MPBX' = 'red')) +
  xlab('Longitude') + ylab('Latitude') +
  # xlim(-122.3475, -122.3325) +
  xlim(min(insar$Long), max(insar$Long)) +
  ylim(min(insar$Lat), max(insar$Lat)) +
  coord_equal() +
  theme_bw(base_size = 18) + 
  theme(legend.position = c(0.75, 0.80)) #,
        # legend.background = element_blank(),
        # legend.box.background = element_rect(colour = "grey"))


pdf("../figure/loc.pdf", width = 14, height = 14)
plot_grid(fig_insar, fig_mp, ncol = 2)
dev.off()

  