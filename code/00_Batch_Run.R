# Batch Run File
rm(list = ls())
setwd("C:/Users/Mark/Dropbox/Graduate School/05) Courses/SI 671/Personal-Finance-Predictions")

cat("Working on 03...")
# source("./code/03_ar_full_prediction.R")
cat("done!\nWorking on 04...")
source("./code/04_prophet_raw_predictions.R")
cat("done!\nWorking on 05...")
source("./code/05_prophet_window_predictions.R")
cat("done!\n")
