# MODEL SETUP ####

set.seed(1)

# set training fraction
train_frac <- 0.7

# Split data -------------------------------------------------------------------

# list mpbx IDs
# list_mpbx <- unique(substr(df_merge$ID, 1, 15))
list_mpbx <- unique(df_merge$ID)
list_train <- sample(list_mpbx, round(train_frac * length(list_mpbx)))
list_test <- setdiff(list_mpbx, list_train)

df_train <- df_merge %>%
  # .[substr(.$ID, 1, 15) %in% list_train, ] %>%
  .[.$ID %in% list_train, ] %>%
  filter(., DistanceHead > -50) %>%
  filter(., DistanceHead <= 100) %>%
  reset_disp(.)
# list of MP measured before TBM passing
considered_ID <- df_train %>% filter(., DistanceHead < 0) %>% .$ID %>% unique()
df_train <- df_train %>% subset(., (ID %in% c(considered_ID)))

df_test <- df_merge %>%
  # .[substr(.$ID, 1, 15) %in% list_test, ] %>%
  .[.$ID %in% list_test, ] %>%
  filter(., DistanceHead > -50) %>%
  filter(., DistanceHead <= 100) %>%
  reset_disp(.)

# Train models -----------------------------------------------------------------

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

# Predict ----------------------------------------------------------------------

# initialize
result_df <- NULL
for (i in 1:length(unique(df_test$ID))) {
  # get ID num
  ID <- unique(df_test$ID)[i]
  # get predictions 
  result <-get_predict(model_lm, model_rf, 
                       df_test %>% 
                         select(-c('ChainageHead')) %>% 
                         select(-c('ChainagePoint')) %>% 
                         # select(-c('ID')) %>% 
                         select(-c('Long')) %>% 
                         select(-c('Lat')) %>% 
                         select(-c('Value')),
                       ID)
  result_df <- rbind(result_df, result)
}

# Prepare results --------------------------------------------------------------

result_df <- result_df %>% 
  # compute absolute error
  mutate(ErrorLM = abs(Actual - PredLM),
         ErrorRF = abs(Actual - PredRF)) %>% 
  # mpbx ID for easier presentation
  mutate(MPBX = paste0('MPBX-', 
                       substr(result_df$ID, 13, 15), '-',
                       substr(result_df$ID, 16, 17))) %>%
  # rename col for labels
  rename(Measured = Actual, OLS = PredLM, RF = PredRF)


ggplot(result_df %>% 
         select(-c('ID')) %>% 
         select(!contains('error')) %>%
         melt(., id = c('ChainageHead', 'MPBX'))) +
  
  geom_line(aes(x = ChainageHead, y = value, color = variable), size = 0.25) +
  geom_point(aes(x = ChainageHead, y = value, color = variable), size = 1) +
  
  geom_vline(xintercept = 0,
             size = 0.25,
             color = 'black',
             linetype = 'dashed') +
  
  scale_x_continuous(name = 'Distance to EPBM Head (m)',
                     limits = c(-50, 100)) +
  scale_y_continuous(name = 'Ground Movements (mm)', breaks_pretty(n = 3)) +
  scale_color_manual(values = c('Actual' = 'black',
                                'OLS' = 'red',
                                'RF' = 'blue')) +
  
  facet_wrap(~MPBX, ncol = 4, scales = 'free',
             labeller = label_wrap_gen(multi_line = FALSE)) +
  
  theme_bw(base_size = 10) +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank(),
    axis.title = element_text(size = 10),
    strip.text = element_text(size = 10),
    # strip.text.y = element_blank(),
    strip.background = element_blank()
  )


# ------------------------------------------------------------------------------

result_selected <- rbind(result_df %>% 
                           # settlement
                           filter(.$MPBX %in% c('MPBX-060-02', 
                                                'MPBX-094-01', 
                                                'MPBX-096-02', 
                                                'MPBX-110-02')) %>% 
                           mutate(Case = 'Settlement'),
                         
                         result_df %>% 
                           # heaving
                           filter(.$MPBX %in% c('MPBX-033-02', 
                                                'MPBX-034-02', 
                                                'MPBX-035-02',
                                                'MPBX-070-02')) %>% 
                           mutate(Case = 'Heaving'),
                         
                         result_df %>% 
                           # steady
                           filter(.$MPBX %in% c('MPBX-037-01',
                                                'MPBX-047-01', 
                                                'MPBX-091-01', 
                                                'MPBX-122-01')) %>% 
                           mutate(Case = 'Stable'))#,
                         
                         # result_df %>% 
                         #   # irregular
                         #   filter(.$MPBX %in% c('MPBX038SR', 
                         #                        'MPBX047SR', 
                         #                        'MPBX048SR')) %>% 
                         #   mutate(Case = 'Irregular'))

result_selected$Case <- factor(result_selected$Case, 
                               levels = unique(result_selected$Case))

