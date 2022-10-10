
# Plot Training Data -----------------------------------------------------------

fig_train <- ggplot(data = df_train) +
  # geom_line(aes(x = HeadDistance, y = Value, color = ID)) +
  # geom_point(aes(x = HeadDistance, y = Value, color = ID)) +
  geom_line(aes(x = HeadDistance, y = DeltaValue, color = ID)) +
  # geom_point(aes(x = HeadDistance, y = DeltaValue, color = ID)) +
  scale_x_continuous(limits = c(-50, 100),
                     breaks = c(-50, -25, 0, 25, 50, 75, 100)) +
  geom_vline(xintercept = 0,
             color = 'black',
             linetype = 'dashed') +
  annotate(geom = 'text',
           label = "EPBM Head",
           x = 0,
           y = -30,
           size = 6,
           angle = 90, vjust = -1, hjust = 0) +
  xlab(' MP to EPBM Head Distance (m)') +
  ylab('Ground Movements from MPBX (mm)') +
  theme_bw(base_size = 18) +
  theme(legend.position = "none")


# Plot Training-Testing Locations ----------------------------------------------
fig_split <- ggplot() +
  # TBM route
  geom_point(data = tbm_pos,  aes(x = Long, y = Lat, color = 'grey'),
             size = 0.5) +
  # MPBX
  geom_point(data = mpbx_loc %>% .[.$ID %in% list_train, ], 
             aes(x = Long, y = Lat, color = 'Train'),
             size = 1.5, alpha = 0.5) +
  geom_point(data = mpbx_loc %>% .[.$ID %in% list_test, ], 
             aes(x = Long, y = Lat, color = 'Test'),
             size = 1.5, alpha = 0.5) +
  scale_color_manual(name = "", 
                     values = c('Train' = 'blue',
                                'Test' = 'red')) +
  xlab('Longitude') + ylab('Latitude') +
  xlim(-122.35, -122.33) +
  # ylim(min(insar$Lat), max(insar$Lat)) +
  coord_equal() +
  theme_bw(base_size = 18) + 
  theme(legend.position = c(0.75, 0.80),
        legend.title = element_blank()) #,
# legend.background = element_blank(),
# legend.box.background = element_rect(colour = "grey"))

# Save Figures as PDF ----------------------------------------------------------

pdf("../figs/split.pdf", width = 14, height = 14/1.5)
ggarrange(fig_split, fig_train, ncol = 2, widths = c(1, 2))
dev.off()
