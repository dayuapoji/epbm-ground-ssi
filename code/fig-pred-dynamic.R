# FIGURE DYNAMIC PREDICTION ####################################################
i <- 16

mae_list <- NULL
# loop over chainage head
for (i in 1:length(unique(result_df$ChainageHead))) {
  
  # get chainage (smallest chainage within the range)
  chainage <- unique(result_df$ChainageHead)[i]
  
  # get data from mp measurements
  df_mp <- df_merge %>%
    # select a chainage head for each interation
    .[.$DistanceAlignment <= 6, ] %>%
    .[.$DistanceCrown == 3.048, ] %>%
    filter(., DistanceHead > -50) %>%
    # filter(., DistanceHead <= 500) %>%
    reset_disp(.) 
  # list of MP measured before TBM passing
  considered_ID <- df_mp %>% filter(., DistanceHead < 0) %>% .$ID %>% unique()
  df_mp <- df_mp %>% 
    subset(., (ID %in% c(considered_ID))) %>%
    .[.$ChainageHead == chainage, ] %>%
    .[.$ChainagePoint >= range_chainage[1] & 
        .$ChainagePoint <= range_chainage[2], ]
  
  # get TBM data
  df_tbm <- df_merge %>% 
    # retain only chainage head and TBM data
    .[ , c(1, 11:ncol(.))] %>% unique() %>%
    # scale data for presentation purpose
    mutate_at(c(2:ncol(.)), scale) #%>%
  
  # get MAE
  df_mae <- merge(result_df %>% .[.$ChainageHead == chainage, ] %>%
                    # retain data prediction at current step (ahead until 150 m behind tbm head)
                    .[.$ChainagePoint >= chainage - 100/0.3048, ],
                  df_mp[ , c('ChainagePoint', 'DeltaValue')] %>%
                    mutate(ChainagePoint = round(ChainagePoint)),
                  by = 'ChainagePoint')
  
  mae_step <- data.frame(ChainageHead = chainage,
                         MAE = mae_vec(df_mae$DeltaValue, df_mae$PredRF)) 
  
  mae_list <- rbind(mae_list, mae_step)
  
  # plot
  fig_ground <- ggplot() +
    
    geom_point(data = df_mp,
               mapping = aes(x = ChainagePoint, y = DeltaValue, color = ID),
               size = 1) +
    geom_segment(data = df_mp,
                 mapping = aes(x=ChainagePoint, xend=ChainagePoint,
                               y=0, yend=DeltaValue, color = ID)) +
    
    geom_point(data = result_df %>%
                 # select a chainage head for each interation
                 .[.$ChainageHead == chainage, ], # %>%
               # group by chainage head, then shift tail to zero
               # group_by(ChainageHead) %>%
               # mutate(PredRF = PredRF - tail(PredRF, 1)),
               mapping = aes(x = ChainagePoint, y = PredRF),
               color = 'black', alpha = 0.5, 
               size = 0.25) +
    
    # geom_point(aes(x = ChainagePoint, y = Actual),
    #           size = 2) +
    
    # annotation
    annotate("rect", fill = 'red', alpha = 0.1,
             xmin = max(26000, chainage - 100 / 0.3048), 
             xmax = min(28200, chainage + 50 / 0.3048),
             ymin = -22, ymax = 0) +
    
    annotate("rect", fill = 'black',
             xmin = (chainage - (20 / 0.3048)), xmax = chainage,
             ymin = -22, ymax = -20) +
    annotate("rect", fill = 'grey',
             xmin = range_chainage[1], xmax = (chainage - (20 / 0.3048)),
             ymin = -22, ymax = -20) +

    geom_vline(xintercept = chainage,
               color = 'black',
               linetype = 'dashed') +
    
    # annotate(geom = 'text',
    #          label = "EPBM Head",
    #          x = 0,
    #          y = -6.5,
    #          angle = 90, vjust = -1, hjust = 0) +
    # setting
    # ggtitle(paste("Chainage at EPBM Head =", chainage_list[i])) +
    xlim(range_chainage[1], range_chainage[2]) +
    ylim(-22, 2) +
    xlab('Chainage (ft)') + ylab('Ground Movements (mm)') +
    theme_bw(base_size = 8) +
    theme(legend.position = 'none',
          panel.grid = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank())
  

  fig_data <- 
    ggplot() +
    geom_line(data = df_tbm %>% 
                melt(., id = 'ChainageHead') %>%
                .[.$ChainageHead <= chainage, ],
              mapping = aes(x = ChainageHead, y = value, color = variable),
              size = 0.25, alpha = 0.5) +
    
    geom_point(data = df_tbm %>%
                 melt(., id = 'ChainageHead') %>%
                 .[.$ChainageHead == chainage, ],
               mapping = aes(x = ChainageHead, y = value, color = variable),
               size = 0.25, alpha = 0.5) +

    geom_text_repel(data = df_tbm %>%
                      melt(., id = 'ChainageHead') %>%
                      .[.$ChainageHead == chainage, ],

                    mapping = aes(x = ChainageHead, y = value,
                                  label = variable,
                                  color = variable),
                    size = 6 * 25.4/72.27,
                    segment.size = 0.25, segment.alpha = 0.5,
                    nudge_x = 250, nudge_y = 2.5) +

    xlim(range_chainage[1], range_chainage[2]) +
    ylim(-4, 4) +
    xlab('Chainage (ft)') + ylab('Scaled EPBM Data') +
    theme_bw(base_size = 8) +
    theme(legend.position = 'none',
          panel.grid = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank())

  fig_mae <- ggplot(mae_list) +
    geom_point(aes(x = ChainageHead, y = MAE), color = 'red') +
    geom_line(aes(x = ChainageHead, y = MAE), size = 0.25, color = 'red') +
    
    xlim(range_chainage[1], range_chainage[2]) +
    ylim(0, 5) +
    xlab('Chainage (ft)') + ylab('MAE') +
    theme_bw(base_size = 8) +
    theme(legend.position = 'none',
          panel.grid = element_blank())
  
  figs <- ggarrange(fig_ground, fig_data, fig_mae, nrow = 3)
  ggsave2(paste0('../output/figs-anim/pred_dynamic_', i, '.png'), plot = figs,
          width = 7.5, height = 7.5/1.5, units = c("in"))
  
  # pdf(file = paste0('../output/figs-anim/pred_dynamic_', i, '.pdf'),
  #     width = 7.5, height = 7.5/1.5)
  # ggarrange(fig_ground, fig_data, fig_mae, nrow = 3)
  
}

for (i in 1:(1+length(unique(result_df$ChainageHead)))) {dev.off()}

