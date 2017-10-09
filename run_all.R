# TCI index and analyses
#
#  setwd('path/to/TCI')

# get assumptions
source('assumptions.R')

# outputs directory (creates one per day!)
now <- Sys.time()
dname <- paste0( 'run_', strftime(now, format = "%Y%m%d"))
outdir <- file.path( 'outputs', dname )
dir.create(outdir)
dir.create(file.path(outdir,'tables'))
dir.create(file.path(outdir,'figures'))

# view assumptions / parameters
print(ls.str())

# 1. compile data and calculate TCI scores

source('code/0_getData.R')

# 2. summary maps and tables

source('code/maps.R')
source('code/tables.R')
source('code/figures.R')

# 3. Analyses 

source('code/1_uncertainty.R')
source('code/2_spatial_autocorrelation.R')
source('code/3_cluster.R')

