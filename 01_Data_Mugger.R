# File for processing YNAB Data

library(readr)
library(plyr)
library(lubridate)
library(magrittr)
library(dplyr)
library(ggplot2)
library(tidyr)

 setwd("C:/Users/mtkur/Dropbox/Graduate School/05) Courses/SI 671/Personal-Finance-Predictions")

data_20181112 <- read_csv("./data_20181112.txt")