# RF HYPERPARAMETER TUNING #####################################################

set.seed(1)


# Split data -------------------------------------------------------------------

# set train frac
train_frac <- 0.7

# list mpbx IDs
list_mpbx <- unique(df_merge$ID)
list_train <- sample(list_mpbx, round(train_frac * length(list_mpbx)))
# list_test <- setdiff(list_mpbx, list_train)

df_train <- df_merge %>%
  .[.$ID %in% list_train, ] %>%
  filter(., DistanceHead > -50) %>%
  filter(., DistanceHead <= 100) %>%
  reset_disp(.)


# Tuning -----------------------------------------------------------------------

# set training control
ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 5,
                     verboseIter = FALSE,
                     search = 'grid')

# cross validation
cv_result <- tune_rf(df_train %>% 
                      select(-c('ChainageHead')) %>% 
                      select(-c('ChainagePoint')) %>% 
                      select(-c('ID')) %>% 
                      select(-c('Long')) %>% 
                      select(-c('Lat')) %>% 
                      select(-c('Value')),
                    ctrl)
  
  

