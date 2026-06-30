
## Convert data to model format, write model input files

## Before: intercatch (all years), lengths from IC (all years), whiting DATRAS object (all years)
## After:  input.RData (input)

library(icesTAF)

mkdir("input")

## Get data
lfs <- readRDS(paste0("data/lfq_2022_",ly,"_raised_allocated.rds")) # get lengths commercial fish - IC
load("data/WhitingData_subset_noCorrection.RData")
alldat <- readRDS("data/whg.27.3a_catch_data_raised_discards_2002_2025_alt.Rds")

save(lfs, d, alldat, file="input/input.RData")
