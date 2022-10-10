# FEATURE IMPORTANCE ###########################################################

set.seed(1)

# Overall ----------------------------------------------------------------------

# define segements
boundary_df <- data.frame(Segment = c(1),
                          SegmentStart = c(-50),
                          SegmentEnd =   c(100))

# compute feature importance
varimp_overall <- get_varimp_segment(df_merge, boundary_df, tbm_list)

# save
# write_csv(varimp_overall, paste0('../output/varimp_overall.csv'))



# Detailed ---------------------------------------------------

# define segments
boundary_df <- data.frame(Segment = c(1, 2, 3, 4, 5),
                          SegmentStart = c(-50, 3, 8, 18, 30),
                          SegmentEnd =   c(3, 8, 18, 30, 100))

# compute feature importance
varimp_segment <- get_varimp_segment(df_merge, boundary_df, tbm_list)

# save
# write_csv(varimp_segment, paste0('../output/varimp_segment.csv'))


# ------------------------------------------------------------------------------
# set factor level
# varimp$Case <- factor(varimp$Case, levels = unique(varimp$Case))
# varimp$Method <- factor(varimp$Method, levels = unique(varimp$Method))
