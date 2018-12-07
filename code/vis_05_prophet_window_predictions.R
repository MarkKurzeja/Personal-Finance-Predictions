setwd("C:/Users/Mark/Dropbox/Graduate School/05) Courses/SI 671/Personal-Finance-Predictions")

library(readr)
library(ggplot2)
library(plyr)
library(dplyr)


prophet_window <- read_csv("saved_runs/finished/05_prophet_results_window.csv")

prophet_window_sum <- prophet_window %>%
  group_by(interval_length) %>%
  summarize(error = mean(error))



# Plottings
prophet_window_sum %>%
  ggplot() +
  # facet_wrap(~model, scales = "free") +
  # scale_y_continuous(limits = c(-1000,1000)) +
  geom_point(aes(x = interval_length, y = error), alpha = 1) +
  # geom_bin2d(aes(x = interval_length, y = error, group = interval_length)) +
  # geom_density_2d()
  # scale_y_continuous(limits = c(-150,400)) + 
  geom_smooth(aes(x = interval_length, y = error)) +
  ggtitle("Windowed Sum Predictions using Prophet",
          "Made predictions using 30 day sum instead of raw values") +
  labs(x = "Number of Training Points", y = "Absolute Error") +
  geom_hline(linetype =2, yintercept = 0)

ggsave("./figures/05_window_prophet_predictions.png", device = "png", width = 8, height = 2.5, dpi = 1200)

