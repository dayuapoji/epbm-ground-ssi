# LOAD DATA ####################################################################

# path to data folder
path <- '../../../../2-data/seattle/'


# TBM --------------------------------------------------------------------------

# switch to 1st edition since newer version of readr mess up non-ASCII 
readr::local_edition(1)

# data TBM
tbm_orig <- read_csv(paste0(path,'SR99.csv'))

# tunnel depth
tunnel_depth <- read_csv('../data/tunnel_depth.csv')

# Monitoring points ------------------------------------------------------------

# MP coordinates
coord_orig <- read_csv(paste0(path,'mp/AWV_coordinate_new.csv'))

# MPBX depth (meter)
mpbx_depth <- read_csv('../data/mpbx_depth.csv')

# MPBX data 
mpbx_orig <- NULL
for (i in 1:6) {
  mpbx_i <- read_csv(paste0(paste0(path,'mp/07_',i,'.csv')))
  mpbx_orig <- rbind(mpbx_orig, mpbx_i)
}

