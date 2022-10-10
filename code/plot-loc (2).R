# PLOT MP LOCATIONS ####

# Plot MPs ---------------------------------------------------------------------

ggplot() +
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
  xlim(min(insar_center$Long), max(insar_center$Long)) +
  ylim(min(insar_center$Lat), max(insar_center$Lat)) +
  coord_equal() +
  theme_bw() +
  theme(legend.position = c(0.75, 0.85),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "grey"))


# Plot InSAR -------------------------------------------------------------------

ggplot() +
  geom_point(data = insar_center, aes(Long, Lat), color = 'red') +
  geom_point(data = tbm_pos, aes(Long, Lat), size = 0.5, color = 'grey') +
  coord_equal() +
  theme_bw()
