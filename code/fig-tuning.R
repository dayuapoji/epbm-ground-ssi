# FIGURE HYPERPARAMETER TUNING =================================================

text_size <- 10
text_size_label <- 8

# plot figure ntrees -----------------------------------------------------------
fig_ntrees <- ggplot(data = cv_result %>%
                       select(!contains('splitrule')) %>%
                       select(!contains('MSE')) %>%
                       # select(!contains('MAE')) %>%
                       select(!contains('SD')) %>%
                       .[.$min.node.size == 1, ],
                     
                     mapping = aes(x = ntrees, y = MAE, group = mtry)) +
  
  geom_line(aes(color = factor(mtry)), size = 0.25) +
  geom_point(aes(color = factor(mtry)), size = 1) +
  
  geom_text_repel(data = cv_result %>% 
                    select(!contains('splitrule')) %>%
                    select(!contains('MSE')) %>%
                    # select(!contains('MAE')) %>%
                    select(!contains('SD')) %>%
                    .[.$min.node.size == 1, ] %>%
                    .[.$ntrees == 600, ],
                  
                  mapping = aes(x = ntrees, y = MAE,
                                label = paste('mtry = ',mtry),
                                color = factor(mtry)),
                  size = text_size_label * 25.4/72.27, #2.5,
                  segment.size = 0.25, segment.alpha = 0.5,
                  nudge_x = -10, nudge_y = 0.025) +
  
  ggtitle('Minimum Node Size = 1') +

  scale_x_continuous(n.breaks = 10) +
  labs(x = "Number of Trees (ntrees)", y = expression("MAE (mm)")) +
  
  theme_bw(base_size = text_size) +
  theme(plot.title = element_text(size = text_size),
        axis.title = element_text(size = text_size),
        panel.grid = element_blank(),
        legend.position = 'none') 

# plot figure mtry -------------------------------------------------------------
fig_mtry <-ggplot(data = cv_result %>% 
                    select(!contains('splitrule')) %>%
                    select(!contains('MSE')) %>%
                    # select(!contains('MAE')) %>%
                    select(!contains('SD')) %>%
                   .[.$ntrees == 500, ],
                 mapping = aes(x = mtry, y = MAE, group = min.node.size)) +
  
  geom_line(aes(color = factor(min.node.size)), size = 0.25) +
  geom_point(aes(color = factor(min.node.size)), size = 1) +
  
  geom_text_repel(data = cv_result %>% 
                    select(!contains('splitrule')) %>%
                    select(!contains('MSE')) %>%
                    # select(!contains('MAE')) %>%
                    select(!contains('SD')) %>%
                    .[.$mtry == max(.$mtry), ] %>%
                    .[.$ntrees == 500, ],
                  
                  mapping = aes(x = mtry, y = MAE,
                                label = paste('min node = ',min.node.size),
                                color = factor(min.node.size)),
                  size = text_size_label * 25.4/72.27, #2.5,
                  segment.size = 0.25, segment.alpha = 0.5,
                  force = 10) +

  ggtitle('Number of Trees = 500') +
  
  scale_x_continuous(n.breaks = 10) +
  labs(x = "Number of Features at Each Split (mtry)", y = expression("MAE (mm)")) +
  
  theme_bw(base_size = text_size) +
  theme(plot.title = element_text(size = text_size),
        axis.title = element_text(size = text_size),
        panel.grid = element_blank(),
        legend.position = 'none') 


# save figure
pdf(file = paste0('../output/figs/tune.pdf'), onefile = FALSE,
    width = 7.5, height = 7.5/2)
ggarrange(fig_ntrees, fig_mtry, nrow = 1)
dev.off()

