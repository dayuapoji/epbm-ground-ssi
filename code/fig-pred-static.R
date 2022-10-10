# FIGURE STATIC PREDICTION #####################################################

# save figure
pdf(file = paste0('../output/figs/pred_static.pdf'),
    width = 7.5, height = 9)

ggplot(result_selected %>% 
         select(-c('ID')) %>% 
         select(!contains('error')) %>%
         melt(., id = c('ChainageHead', 'MPBX', 'Case'))) +
  
  geom_line(aes(x = ChainageHead, y = value, color = variable), size = 0.25) +
  geom_point(aes(x = ChainageHead, y = value, color = variable), size = 1) +
  
  geom_vline(xintercept = 0,
             size = 0.25,
             color = 'black',
             linetype = 'dashed') +
  
  scale_x_reverse(name = 'Distance to EPBM Head (m)',
                  limits = c(100, -50)) +
  scale_y_continuous(name = 'Ground Movements (mm)', breaks_pretty(n = 4)) +
  scale_color_manual(values = c('Measured' = 'black',
                               'OLS' = 'red',
                               'RF' = 'blue')) +

  facet_wrap(Case~MPBX, nrow = 4, scales = 'free', dir = 'v',
             labeller = label_wrap_gen(multi_line = FALSE)) +
  
  theme_bw(base_size = 10) +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank(),
    legend.position = 'bottom',
    axis.title = element_text(size = 10),
    strip.text = element_text(size = 10),
    # strip.text.y = element_blank(),
    strip.background = element_blank()
  )

dev.off()


# FIGURE STATIC PREDICTION ERROR ###############################################

# save figure
pdf(file = paste0('../output/figs/pred_static_errors.pdf'),
    width = 7.5, height = 7.5/1.5)

ggplot(result_df) +
  
  geom_point(aes(x = ChainageHead, y = ErrorLM, color = 'OLS'), 
            alpha = 0.5) +
  geom_point(aes(x = ChainageHead, y = ErrorRF, color = 'RF'), 
             alpha = 0.5) +
  
  geom_smooth(aes(x = ChainageHead, y = ErrorLM, color = 'OLS'), 
              method = 'loess', se = F, 
              size = 1) +
  geom_smooth(aes(x = ChainageHead, y = ErrorRF, color = 'RF'), 
              method = 'loess', se = F, 
              size = 1) +
  
  geom_vline(xintercept = 0,
             size = 0.25,
             color = 'black',
             linetype = 'dashed') +
  
  scale_color_manual(values = c('OLS' = 'red', 'RF' = 'blue')) +
  
  scale_x_reverse(name = 'Distance to EPBM Head (m)', 
                  limits = c(100, -50),
                  n.breaks = 20) +
  scale_y_continuous(name = 'Absolute Error (mm)') + 
  
  theme_bw(base_size = 10) +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = c(0.85, 0.85))

dev.off()

