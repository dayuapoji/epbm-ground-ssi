# LONGITUDINAL SEGMENT ANALYSIS ################################################

set.seed(1)

# Split data -------------------------------------------------------------------

# set training fraction
train_frac <- 0.8
list_mpbx <- unique(df_merge$ID)
list_train <- sample(list_mpbx, round(train_frac * length(list_mpbx)))
list_test <- setdiff(list_mpbx, list_train)

# Setting ----------------------------------------------------------------------

# set error threshold for boundary
error_threshold <- 0.2

# initial boundary
boundary <- -50

# store results
boundary_vec <- NULL
error_segment_df <- NULL

# segment counter
s <- 1

# Compute ----------------------------------------------------------------------

# end loop when boundary 100
while (boundary < 100) {
  
  # initialize error segment vector
  error_segment_vec <- NULL
  
  # try every boundary from initial segment boundary to end (100)
  for (b in (boundary + 2):100) {
    
    # set df according to segment 
    df_segment <- df_merge %>%
      filter(., DistanceHead > boundary) %>%
      filter(., DistanceHead <= b) %>%
      reset_disp(.)
    
    # split data
    df_train <- df_segment %>% .[.$ID %in% list_train, ]
    df_test <- df_segment %>% .[.$ID %in% list_test, ]
    
    # training
    model_lm <- lm(DeltaValue ~ ., data = df_train %>% 
                     select(-c('ChainageHead')) %>% 
                     select(-c('ChainagePoint')) %>% 
                     select(-c('ID')) %>% 
                     select(-c('Long')) %>% 
                     select(-c('Lat')) %>% 
                     select(-c('Value')))
    
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
    
    # initialize error vector
    error_vec <- data.frame(NULL)
    
    for (i in 1:length(df_test$ID)) {
      # get ID num
      ID <- df_test$ID[i]
      
      # get predictions
      result <- get_predict(model_lm, model_rf, 
                                    df_test %>% 
                                      select(-c('ChainageHead')) %>% 
                                      select(-c('ChainagePoint')) %>% 
                                      # select(-c('ID')) %>% 
                                      select(-c('Long')) %>% 
                                      select(-c('Lat')) %>% 
                                      select(-c('Value')),
                                    ID)
      
      # compute errors
      error <- data.frame(ID = ID,
                          MAE_LM = mae_vec(result$Actual, result$PredLM),
                          MAE_RF = mae_vec(result$Actual, result$PredRF))
      error_vec <- rbind(error_vec, error)
    }
    
    # get error segment 
    error_segment <- data.frame(Boundary = b,
                                MAE_LM = mean(error_vec$MAE_LM),
                                MAE_RF = mean(error_vec$MAE_RF))
    error_segment_vec <- rbind(error_segment_vec, error_segment)
    
    # computation control
    print(paste('b =', b, 'done'))
  }
  
  # get next boundary based on MAE difference
  # boundary <- error_segment_vec %>%
  #   mutate(Diff_RF = c(diff(.$MAE_RF, lag = 1), 0)) %>%
  #   .[.$Diff_RF > error_threshold, ] %>%
  #   head(1) %>%
  #   .$Boundary
  # get next boundary based on MAE
  boundary <- error_segment_vec %>%
    .[.$MAE_RF < error_threshold, ] %>%
    tail(1) %>%
    .$Boundary
  
  # catch if nothing in the result
  if (length(boundary) == 0) {boundary <- 100}
  
  # store results
  error_segment_df <- rbind(error_segment_df,
                            error_segment_vec %>% mutate(Segment = s))
  boundary_vec <- append(boundary_vec, boundary)
  
  # computation control
  print(paste('segment =', s, 'boundary = ', boundary))
  
  # increase segment counter
  s <- s + 1
}

error_segment_df$Segment <- paste0('Segment-', error_segment_df$Segment)

boundary_df <- data.frame(Segment = paste0('Segment-',1:length(boundary_vec)),
                          SegmentStart = c(-50, 
                                           boundary_vec[1:(length(boundary_vec)-1)]),
                          SegmentEnd = boundary_vec)

