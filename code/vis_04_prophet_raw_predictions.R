setwd("C:/Users/Mark/Dropbox/Graduate School/05) Courses/SI 671/Personal-Finance-Predictions")


library(readr)
library(ggplot2)
library(plyr)
library(gridExtra)
library(dplyr)

prop_data <- read_csv("saved_runs/finished/04_prophet_results_raw.csv")

prop_data_sum <- prop_data %>%
  group_by(interval_length) %>% 
  summarize(
    sd = sd(error),
    upper = quantile(error, probs = 0.95), 
    lower = quantile(error, probs = 0.05),
    error = mean(error), 
    count = n()
    )

# prop_data %>% filter(interval_length == 45) %>% {quantile(.$error, .05)}

p1 <-
  prop_data_sum %>%
  ggplot() +
  geom_point(aes(x = interval_length, y = error)) + 
  # geom_ribbon(aes(x = interval_length, ymin = lower, ymax = upper), alpha = 0.2) +
  # scale_y_continuous(limits = c(-800, 800)) +
  geom_hline(linetype = 2, yintercept = 0) +
  ggtitle("Facebook Prophet Predictions on Raw Time Series",
          "This method is predicting on the zero-inflated time series averaged over each prediction interval") +
  # labs(x = "Number of Training Points", y = "Absolute Error")
  labs(x = "", y = "Absolute Error")

p2 <-
  prop_data_sum %>%
  ggplot() +
  geom_point(aes(x = interval_length, y = error)) + 
  geom_ribbon(aes(x = interval_length, ymin = lower, ymax = upper), alpha = 0.2) +
  scale_y_continuous(limits = c(-800, 800)) +
  geom_hline(linetype = 2, yintercept = 0) +
  ggtitle("Facebook Prophet Predictions on Raw Time Series - 90% Confidence Intervals") +
  labs(x = "Number of Training Points", y = "Absolute Error")

ggsave(plot = grid.arrange(p1,p2), filename = "./figures/prophet_errors_w_zeros.png", device = "png", width = 8, height = 4, dpi = 1200)
ggsave(plot = p1, filename = "./figures/prophet_errors_w_zeros_wo_variance.png", device = "png", width = 8, height = 4, dpi = 1200)
prop_data %>%
  ggplot() +
  geom_point(aes(x = true_value, y = expected, color = interval_length)) +
  geom_abline(slope = 1, intercept = 0, color = "blue") +
  scale_y_continuous(limits = c(0, 800)) +
  scale_color_gradientn(colors = rev(RColorBrewer::brewer.pal(5, "RdYlBu"))) + 
  ggtitle("Facebook Prophet Expected Vs True Distribution ") +
  labs(x = "Actual Value", y = "Predicted Value")

ggsave(filename = "./figures/prophet_expected_vs_true.png", device = "png", width = 8, height = 2.5, dpi = 1200)
