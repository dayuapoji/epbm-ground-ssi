# FIGURE FEATURE IMPORTANCE ####################################################


# Fig segment ------------------------------------------------------------------

text_size <- 8


# plot
fig_segment <- ggplot(varimp_segment %>%
                        .[.$Importance >= 0, ] %>%
                        group_by(Segment) %>%
                        slice_head(n = 8) %>%
                        ungroup %>%
                        mutate(Group = factor(.$Group, levels = c("Excavation", 
                                                                  "Advancing", 
                                                                  "Steering",
                                                                  "Ground Conditioning",
                                                                  "Earth Pressure Balancing",
                                                                  "Muck Extraction",
                                                                  "Tail Grouting",
                                                                  "Spatial Geometries"))) %>%
                        mutate(Feature = reorder_within(Feature, 
                                                        Importance, 
                                                        list(Segment))) %>% 
                        mutate(Segment = factor(.$Segment, 
                                                levels = rev(unique(.$Segment))))) +
  
  geom_col(aes(x = Importance, 
               y = Feature,
               fill = Group),
           show.legend = FALSE) +
  
  scale_fill_brewer(palette = "Set2") +
  scale_x_continuous(n.breaks = 3) +
  scale_y_reordered() +
  
  xlab('Error Increase due to Permutation (mm)') +
  
  # coord_flip() +
  
  facet_wrap(~ Segment, nrow = 1, scales = 'free') +
  
  theme_bw(base_size = text_size) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size = text_size),
        # axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid = element_blank(),
        # strip.text = element_text(size = text_size),
        # strip.text.y = element_blank(),
        strip.background = element_blank())


# ground movement segments -----------------------------------------------------

fig_ground <- ggplot(df_merge %>%
                       filter(., DistanceHead > -50) %>%
                       filter(., DistanceHead <= 100) %>%
                       reset_disp(.)) +
  
  geom_line(aes(x = DistanceHead, y = DeltaValue, group = ID, ), 
            size = 0.25, color = 'black', alpha = 0.5,
            show.legend = FALSE) +
  # geom_smooth(aes(x = DistanceHead, y = DeltaValue, fill = 'red'), 
  #             method = 'loess', se = F,
  #             size = 0.5, color = 'red',
  #             show.legend = FALSE) +
  
  # annotate boundary
  geom_vline(xintercept = c(boundary_df$SegmentStart, 100), 
             size = 0.25, linetype = 'dashed', color = 'red') +
  annotate('text', label = c('  1', '  2', '  3', '  4', '  Segment-5'),
           x = (c(boundary_df$SegmentEnd)),
           y = rep(4.5, 5),
           hjust = 0,
           size = text_size * 25.4/72.27) +
  
  # annotate EPBM and tunnel
  annotate("rect", fill = 'grey',
           xmin = 20, xmax = 100,
           ymin = -25.5, ymax = -22.5) +
  annotate("rect", color = 'black', fill = 'white',
           xmin = 0, xmax = 20,
           ymin = -25.5, ymax = -22.5) +
  
  annotate('text', label = 'Tunnel', x = 60, y = -24, color = 'black', 
           size = text_size * 25.4/72.27) +
  annotate('text', label = 'EPBM', x = 10, y = -24, color = 'black', 
           size = text_size * 25.4/72.27) +
  
  
  scale_x_reverse(name = 'Distance to EPBM Head (m)', n.breaks = 15,
                  limits = c(100, -50)) +
  scale_y_continuous(name = 'Ground Movements (mm)') +
  
  theme_bw(base_size = text_size) + 
  theme(legend.position = 'none',
        panel.grid = element_blank())


# save figure
pdf(file = paste0('../output/figs/varimp_segment.pdf'), onefile = F,
    width = 7.5, height = 7.5/1.5)
ggarrange(fig_ground, fig_segment, nrow = 2, heights = c(1, 0.5))
dev.off()


# Fig overall ------------------------------------------------------------------

text_size <- 10

# save figure
pdf(file = paste0('../output/figs/varimp_overall.pdf'),
    width = 7.5, height = 7.5/1.5)

# plot
ggplot(varimp_overall %>%
         # .[.$Importance >= 0, ] %>%
         # slice_head(n = 10) %>%
         # ungroup %>%
         mutate(Group = factor(.$Group, levels = c("Excavation", 
                                                   "Advancing", 
                                                   "Steering",
                                                   "Ground Conditioning",
                                                   "Earth Pressure Balancing",
                                                   "Muck Extraction",
                                                   "Tail Grouting",
                                                   "Spatial Geometries"))) %>%
         mutate(Feature = reorder_within(Feature, Importance, list(Segment)))) +
  
  geom_col(aes(x = Importance, 
               y = Feature,
               fill = Group)) +
  
  scale_fill_brewer(palette = "Set2") +
  scale_y_reordered(limits = rev) +
  
  xlab('Error Increase due to Permutation (mm)') +
  
  coord_flip() +
  
  # facet_wrap(~ Segment, nrow = 1, scales = 'free') +#,
  # labeller = label_wrap_gen(multi_line = FALSE)) +
  
  theme_bw(base_size = text_size) +
  theme(legend.title = element_blank(),
        legend.position = c(0.85, 0.65),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = text_size),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid = element_blank(),
        # strip.text = element_text(size = text_size),
        # strip.text.y = element_blank(),
        strip.background = element_blank())

dev.off()