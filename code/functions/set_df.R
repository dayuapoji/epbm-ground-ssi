set_df <- function(df) {
  df <- df %>%
    .[-c(1), ] %>%
    lapply(., as.numeric) %>%
    as_tibble(.)
  return(df)
}