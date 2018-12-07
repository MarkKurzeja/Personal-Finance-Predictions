setwd("C:/Users/Mark/Dropbox/Graduate School/05) Courses/SI 671/Personal-Finance-Predictions")


library(readr)

null_data <- read_csv("saved_runs/finished/08_null_cases.csv")

null_data_sum <- null_data %>%
  group_by(model, interval_length) %>%
  summarize(error = mean(error))

# Plottings
p1 <- null_data %>%
  ggplot() +
  facet_wrap(~model, scales = "free") + 
  geom_point(aes(x = interval_length, y = error), alpha = 0.3, color = "grey30") +
  geom_smooth(aes(x = interval_length, y = error)) +
  ggtitle("Null Case: Using Past Averages and ARIMA to Predict New Points") +
  labs(x = "Number of Training Points", y = "Absolute Error") +
  geom_hline(linetype = 2, yintercept = 0)


p2 <- null_data_sum %>%
  ggplot() +
  facet_wrap( ~ model) +
  geom_point(aes(x = interval_length, y = error)) +
  geom_smooth(aes(x = interval_length, y = error)) +
  ggtitle("Null Case Means: Using Past Averages and ARIMA to Predict Future Outflows") +
  labs(x = "Number of Training Points", y = "Absolute Error") +
  # ggthemes::theme_few() + 
  geom_hline(linetype = 2, yintercept = 0)

ggsave(plot = grid.arrange(p1, p2), "./figures/08_null_errors.png", device = "png", width = 8, height = 8/16 * 9.88, dpi = 1200)
# ggsave("./figures/08_null_errors_means.png", device = "png", width = 8, height = 2.5, dpi = 1200)
