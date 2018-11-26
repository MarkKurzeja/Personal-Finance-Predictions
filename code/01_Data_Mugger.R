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

# Grouping by month
mdat <- ndat %>% 
  group_by(Year = year(Date), Month = month(Date)) %>%
  summarize(Monthly = sum(Outflow))

# Plot for the project
mdat %>% 
  mutate(Date = lubridate::make_date(Year, Month, 1)) %>% 
  ggplot() + 
    geom_line(aes(Date, Monthly)) + 
  ggtitle("Spending on One Particular Category since July 2016") +
  labs(y = "Monthly Spending") + 
  ggsave(path = "./figures/", device = "png", filename = "Spending_Example.png")

# Loss function
myloss <- function(actual, error) {
  return(abs(actual - error))
}

# Baselines - Lookback Average
all_opts = expand.grid(mend = 5:(length(mdat$Year) - 1), lookback = 0:15)

baseline_results <- adply(.data = all_opts, .margins = 1, .fun = function(x) {
  mend = x$mend
  lookback = min(x$lookback, mend)
  prediction = mean(mdat$Monthly[(mend - lookback):mend])
  data.frame(pred = prediction, actual = mdat$Monthly[mend + 1], Type = sprintf("Mean(lookback = %i)", x$lookback))
})

baseline_results %<>% 
  mutate(error = myloss(actual, pred)) %>% 
  group_by(Type) %>% 
  summarize(error = mean(error))

xtable::xtable(baseline_results)

baseline_results %>% 
  ggplot() + 
  geom_point(aes(y = fct_rev(Type), x = error)) + 
  labs(y = "Type", x = "Mean Error") + 
  ggtitle("Mean Prediction Error for Baseline", "Using the Mean to Predict k + 1")
  ggsave(path = "./figures/", device = "png", filename = "Lagged_Mean_Prediction_Results.png")

# Arima Output 
all_opts = expand.grid(mend = 5:(length(mdat$Year) - 1), one = c(0,1), two = 0, three = 0:2)

arima_results <- adply(.data = all_opts, .margins = 1, .fun = function(x) {
  mend = x$mend
  mmod <- arima(x = mdat$Monthly[1:mend], order = c(x$one,x$two,x$three))
  prediction = predict(mmod,1)$pred[1]
  data.frame(pred = prediction, actual = mdat$Monthly[mend + 1], Type = sprintf("Arima(%i,%i,%i)", x$one, x$two, x$three))
})

arima_results %<>% 
  mutate(error = myloss(actual, pred)) %>% 
  group_by(Type) %>% 
  summarize(error = mean(error))

arima_results %>% 
  ggplot() + 
  geom_point(aes(x = error, y = Type)) +
  ggtitle("Arima Modeling Error")
ggsave(path = "./figures/", device = "png", filename = "Arima_Modeling_Error_Init.png")

