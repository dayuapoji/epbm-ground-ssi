# FIGURE LONGITUDINAL SEGMENT ANALYSIS #########################################

# save figure
pdf(file = paste0('../output/figs/segment.pdf'),
    width = 7.5, height = 7.5/1.5)

ggplot(error_segment_df %>% 
         left_join(., boundary_df, by = 'Segment')) +# %>%
         # melt(., id = c('Boundary', 'Segment', 
         #                'SegmentStart', 'SegmentEnd'))) +
  
  geom_point(mapping = aes(x = Boundary, y = MAE_RF),
             size = 0.5, color = 'black') +
  geom_line(mapping = aes(x = Boundary, y = MAE_RF),
            size = 0.25, color = 'black') +
  
  geom_vline(mapping = aes(xintercept = SegmentStart), 
             size = 0.25, color = 'red', linetype = 'dashed') +
  geom_vline(mapping = aes(xintercept = SegmentEnd), 
             size = 0.25, color = 'red', linetype = 'dashed') +
  
  # geom_text(data = boundary_df,
  #           mapping = aes(label = "Segment",
  #               x = SegmentStart, y = 1),
  #           size = 8 * 25.4/72.27,
  #           hjust = 0.5, vjust = -0.5, angle = 90) +
  # geom_text(data = boundary_df,
  #           mapping = aes(label = "End",
  #                         x = SegmentEnd, y = 1),
  #           size = 8 * 25.4/72.27, 
  #           hjust = 0.5, vjust = 1.5, angle = 90) +
  

  scale_x_reverse(name = 'Distance to EPBM Head (m)', 
                  limits = c(100, -50),
                  n.breaks = 20) +
  scale_y_continuous(name = 'MAE (mm)', n.breaks = 3) + #, limits = c(0, 3)) +
  
  facet_wrap(~Segment, ncol = 1, strip.position = 'right') +
  
  theme_bw(base_size = 10) +
  theme(panel.grid = element_blank(),
        legend.position = 'none',
        strip.text = element_text(size = 7),
        # strip.text.y = element_blank(),
        strip.background = element_blank())

dev.off()
