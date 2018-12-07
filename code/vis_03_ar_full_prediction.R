setwd("C:/Users/Mark/Dropbox/Graduate School/05) Courses/SI 671/Personal-Finance-Predictions")


library(readr)

ar_full_data <- read_csv("saved_runs/finished/03_ar_full_prediction_results.csv")

ar_full_sum <- ar_full_data %>%
  group_by(model, interval_length) %>%
  summarize(error = mean(error))

# Preprocess the data setting the upper and lower bounds and removing extreamly high and extreme low
# values
# ar_full_prediction_results %<>% filter(!is.na(expected))
# mmedian <- function(x, low = -1000, high = 1000) {median(c(low,high,x))}; mmedian <- Vectorize(mmedian)
# ar_full_prediction_results$expected %<>% mmedian
# ar_full_prediction_results$error %<>% mmedian

# Plottings
ar_full_data %>%
  ggplot() +
  # scale_y_continuous(limits = c(-1000,1000)) +
  geom_point(aes(x = interval_length, y = error, group = lag)) +
  facet_wrap(~model, scales = "free_y") +
  geom_smooth(aes(x = interval_length, y = error)) +
  ggtitle("Model Errors Using Autoregressive Methods and Recursive Computation",
          "Trained sequences include zero") +
  labs(x = "Number of Training Points", y = "Absolute Error") +
  geom_hline(linetype =2, yintercept = 0)

ggsave("./figures/ar_errors_w_zeros.png", device = "png", width = 8, height = 4, dpi = 1200)

ar_full_sum %>%
  ggplot() +
  geom_point(aes(x = interval_length, y = error)) +
  facet_wrap(~model, scales = "free_y") +
  geom_smooth(aes(x = interval_length, y = error)) +
  ggtitle("Mean Model Errors Using Autoregressive Methods",
          "Means around zero are unbiased") +
  labs(x = "Number of Training Points", y = "Absolute Error") +
  geom_hline(linetype =2, yintercept = 0)

ggsave("./figures/ar_errors_w_zeros_means.png", device = "png", width = 8, height = 8/16.4 * 7.5, dpi = 1200)


ar_full_data %>%
  # filter(model == "GLM") %>% 
  ggplot() +
  geom_point(aes(x = true, y = expected)) +
  facet_wrap(~model, scales = "free") +
  geom_abline(slope = 0, intercept = 0, color = "black") +
  geom_smooth(aes(x = true, y = expected)) +
  ggtitle("Model Real Vs Expected Values Using Autoregressive Methods",
          "This analysis included zeros into the lag terms") +
  labs(x = "True Value of Spending", y = "Predicted Value of Spending")

ggsave("./figures/ar_truevexpected_w_zeros.png", device = "png", width = 8, height = 4, dpi = 1200)
