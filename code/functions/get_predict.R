get_predict <- function(model_lm, model_rf, df_test, ID) {
  
  # split data
  df_test <- df_test %>% .[str_detect(.$ID, ID), ]
  
  # predict lm
  pred_lm <- predict(model_lm, newdata = df_test %>% select(-c('ID')))
  
  # predict rf
  pred_rf <- predict(model_rf, data = df_test %>% select(-c('ID')))
  
  # results
  results <- data.frame(ID = ID, 
                        ChainageHead = df_test$DistanceHead,
                        Actual = df_test$DeltaValue, 
                        PredLM = pred_lm, # - pred_lm[1],
                        PredRF = pred_rf$predictions) # - pred_rf$predictions[1])

  return(results)
}


# df_train <- df_segment %>% .[str_detect(.$ID, ID, negate = T), ]
# model_lm <- lm(DeltaValue ~ ., data = df_train[ , 6:ncol(df_train)])
# model_rf <- ranger(DeltaValue ~ ., data = df_train[ , 6:ncol(df_train)])
# ErrorLM = abs(df_test$DeltaValue - pred_lm),
# ErrorRF = abs(df_test$DeltaValue - pred_rf$predictions))