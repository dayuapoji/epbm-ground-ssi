# DYNAMIC PREDICTION ###########################################################

set.seed(1)

range_pred <- c(26000, 28000)
range_chainage <- c(26000, 28200)

# ggplot(df_train) +
#   geom_line(aes(x = DistanceHead, y = DeltaValue, color = ID)) +
#   geom_point(aes(x = DistanceHead, y = DeltaValue, color = ID)) +
#   theme_classic() +
#   theme(legend.position = 'none')


# initialize df for output
result_df <- data.frame(NULL)
result_past <- NULL

for (i in seq(from = range_pred[1], to = range_pred[2], by = 50)) {

  # train model
  df_train <- df_merge %>%
    .[.$ChainageHead <= i, ] %>%
    filter(., DistanceHead > -50) %>%
    filter(., DistanceHead <= 100) %>%
    reset_disp(.)
  
  # list of MP measured before TBM passing
  considered_ID <- df_train %>% filter(., DistanceHead < 0) %>% .$ID %>% unique()
  df_train <- df_train %>% subset(., (ID %in% c(considered_ID)))
  
  # train model
  model_rf <- ranger(DeltaValue ~ ., data = df_train %>% 
                       select(-c('ChainageHead')) %>% 
                       select(-c('ChainagePoint')) %>% 
                       select(-c('ID')) %>% 
                       select(-c('Long')) %>% 
                       select(-c('Lat')) %>% 
                       select(-c('Value')),
                     num.trees = 500,
                     mtry = ncol(df_train)-7, # 6 removed cols + 1 response var
                     min.node.size = 1)

  # create df input
  df_pred <-  df_merge %>% 
    # get the first tbm params within the iteration range
    .[.$ChainageHead >= i, ] %>% head(1) %>%
    # duplicate the tbm params across chainage spatial "monitoring" points
    .[rep(1, length(seq(from = range_chainage[1], 
                        to = range_chainage[2], 
                        by = 1))), ] %>%
    # set monitoring point along the specified chainages
    mutate(ChainagePoint = seq(from = range_chainage[1], 
                               to = range_chainage[2], 
                               by = 1)) %>%
    # approx tunnel depth along the specified chainages
    mutate(DepthTunnel = approx(tunnel_depth$Chainage,
                                tunnel_depth$DepthTunnel,
                                .$ChainagePoint)$y) %>%
    # get mp to tbm head distance
    mutate(DistanceHead = (ChainageHead - ChainagePoint) * 0.3048) %>%
    # set distance form "monitoring" points to the center and crown of tunnel
    mutate(DistanceAlignment = 1) %>%
    mutate(DistanceCrown = 3.048)

  # predictions 
  pred_rf <- predict(model_rf, data = df_pred %>% 
                       select(-c('ChainageHead')) %>% 
                       select(-c('ChainagePoint')) %>% 
                       select(-c('ID')) %>%
                       select(-c('Long')) %>% 
                       select(-c('Lat')) %>% 
                       select(-c('Value')))
  
  # results
  result <- data.frame(ChainageHead = df_pred$ChainageHead,
                       ChainagePoint = df_pred$ChainagePoint,
                       PredRF = pred_rf$predictions - tail(pred_rf$predictions, 1))

  # result_df <- rbind(result_df, result)
  
  # prediction only for ahead until 100 m behind the tbm face
  result_modified <- rbind(result_past %>%
                             .[result$ChainagePoint < unique(result$ChainageHead) - (100 / 0.3048), ],
                           result %>%
                             .[.$ChainagePoint >= unique(.$ChainageHead) - (100 / 0.3048), ]) %>%
    mutate(ChainageHead = unique(result$ChainageHead))

  # save result for the next calc
  result_past <- result_modified
  result_df <- rbind(result_df, result_modified)
  
  print(paste(i,'done'))

}



