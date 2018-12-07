# File for processing YNAB Data
rm(list = ls())
library(readr)
library(plyr)
library(lubridate)
library(magrittr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(forcats)
library(stringr)


setwd("C:/Users/Mark/Dropbox/Graduate School/05) Courses/SI 671/Personal-Finance-Predictions")

data <- read.table("./data/data_20181112.txt", sep = "\t", comment.char = "#", fill = T, header = T)

ndat <- data %>% 
  filter(Sub.Category == "Partying, Drinks, and Food") %>% 
  select(Date, Outflow) %>%
  mutate(Date = lubridate::mdy(Date)) %>% 
  mutate(Outflow = as.character(Outflow))
ndat$Outflow %<>% stringr::str_remove(string = .,pattern =  "[\\$| ]") %>% as.numeric()
ndat %<>% filter(!is.na(Outflow))
# Ensure that the multiple counts are satisified
ndat %<>% 
  group_by(Date) %>% 
  summarize(Outflow = sum(Outflow)) %>% 
  arrange(Date) %>% 
  ungroup()

# Begin by merging the data with all of the dates
all_days = data.frame(Date = seq(0, mdy("11/21/2018") - min(ndat$Date)) + min(ndat$Date))
full_dat <- left_join(all_days, ndat)
full_dat$Outflow[is.na(full_dat$Outflow)] = 0
# Merge multiple days worth of data
full_dat %<>% 
  group_by(Date) %>% 
  summarize(Outflow = sum(Outflow)) %>% 
  arrange(Date) %>% 
  ungroup()



# Begin creating the autoregressive models

# ar_data_full is the autoregressive matrix using lags 
# and has zeros present in the data

# Prediction - keep feeding data back into the model - 
# it should generate a full sequence
ar_data_full <- list()
for (lag in 1:30) {
  ar_data_full[[lag]] = ldply((lag + 1):nrow(full_dat), function(i) {
    d = full_dat$Outflow[seq(i - lag, i - 1)]
    v = full_dat$Outflow[i] 
    data.frame(y = v, lag = t(rev(d)))
  }) 
}

# ar_data_wo_zeros removes the zeros from the previous equation
# which ensures that the measurements are now only on the days

# Prediction - need a seperate model for zeros - iterate
ar_data_wo_zeros <- list()
for (lag in 1:30) {
  ar_data_wo_zeros[[lag]] =
    ldply((lag + 1):nrow(full_dat), function(i) {
      d = ndat$Outflow[seq(i - lag , i - 1)]
      v = ndat$Outflow[i] 
      data.frame(y = v, lag = t(rev(d)))
    }) 
}

# Running Monthly Predictions - this is going to be a running 30 day sum window so 
# that we can remove most of the noise of the predictions of the zeros

window_dat <- ldply(31:nrow(full_dat), function(i) {
  data.frame(Date = full_dat[i, "Date"], Outflow = sum(full_dat$Outflow[seq(i - 30, i - 1)]))
})























