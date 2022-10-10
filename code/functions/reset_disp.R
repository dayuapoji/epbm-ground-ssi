reset_disp <- function(mpbx) {
  
  "Function to reset mp values (i.e., ground deformation) according to 
  the initial value (i.e., choosen distance between TBM head and mp)"
  
  # initialize list
  mpbx_list <- NULL
  # loop over each instrument IDs
  for (i in 1:length(unique(mpbx$ID))) {
    mpbx_i <- mpbx %>% 
      # select 1 instrument ID
      .[.$ID == unique(.$ID)[i], ] %>% 
      # normalize values to "initial" value
      mutate(DeltaValue = Value - Value[1])
    # row bind the results
    mpbx_list <- rbind(mpbx_list, mpbx_i)
  }
  return(mpbx_list)
}
