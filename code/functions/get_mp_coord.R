get_mp_coord <- function(coord, instrument) {
  # function to get coordinates of instruments
  # INPUT
  #   coord      : coordinate list of all instr
  #   instrument : 3 (NSSP), 4 (ARSP), 6 (Piezo), 7 (MPBX)

  mp_coord <- coord %>%
    # filter instrument 
    filter(str_sub(ID, 2, 2) == instrument)
  
  return(mp_coord)
}

