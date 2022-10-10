# SETUP ####


# Library ----------------------------------------------------------------------

library(rstudioapi) # setup
library(tidyverse)  # data processing
library(magrittr)
library(reshape2)
library(sf)         # spatial analysis
library(sp)
library(gstat)
library(geosphere)
library(parallel)   # parallel computing
library(foreach)
library(doParallel)
library(tidymodels) # machine learning
library(caret)
library(ranger)
library(cowplot)    # plotting
library(egg)
library(tidytext)
library(ggrepel)
# library(png)


# Set Working Directory --------------------------------------------------------

# set current directory as working directory
setwd(dirname(getActiveDocumentContext()$path))


# Functions --------------------------------------------------------------------

# list of functions
functions <- list.files(path = 'functions/', pattern = "[.]R$", 
                        full.names=TRUE, recursive = TRUE)

# load
for (i in (1:length(functions))) {
  print(functions[i])
  source(functions[i])
}


