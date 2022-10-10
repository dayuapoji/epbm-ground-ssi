
get_varimp_rf <- function(df) {
  
  # train model
  model <- ranger(DeltaValue ~ ., data = df,
                  num.trees = 500, 
                  mtry = ncol(df)-1,
                  min.node.size = 1,
                  importance = 'permutation')
  
  # extract coeff
  varimp <- model$variable.importance %>% data.frame(.) 
  
  # prepare varimp df
  varimp <- data.frame(Feature = rownames(varimp),
                       Importance = varimp[ , 1]) %>% 
    # standardize importance to max = 1
    # mutate(., Importance = round(Importance / max(Importance), 3)) %>%
    # arrange from high to low importance
    arrange(desc(Importance))  %>%
    # set factor for plot order
    mutate(Feature = factor(Feature, levels = Feature[order(Importance)]))
  
  return(varimp)
}
