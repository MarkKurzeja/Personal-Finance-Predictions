setwd("C:/Users/Mark/Dropbox/Graduate School/05) Courses/SI 671/Personal-Finance-Predictions")


library(readr)

yinyang_data <- read_csv("saved_runs/finished/06_yin_yang_prediction_results.csv")

yinyang_sum <- yinyang_data %>%
  group_by(interval_length) %>%
  summarize(error = mean(error))

# Preprocess the data setting the upper and lower bounds and removing extreamly high and extreme low
# values
# ar_full_prediction_results %<>% filter(!is.na(expected))
# mmedian <- function(x, low = -1000, high = 1000) {median(c(low,high,x))}; mmedian <- Vectorize(mmedian)
# ar_full_prediction_results$expected %<>% mmedian
# ar_full_prediction_results$error %<>% mmedian

# Plottings
yinyang_sum %>%
  ggplot() +
  # scale_y_continuous(limits = c(-1000,1000)) +
  geom_point(aes(x = interval_length, y = error), alpha = 1) +
  # geom_bin2d(aes(x = interval_length, y = error, group = interval_length)) +
  # geom_density_2d()
  scale_y_continuous(limits = c(-150,400)) + 
  geom_smooth(aes(x = interval_length, y = error)) +
  ggtitle("Estimating the Joint Distribution of Zeros & Amounts Using Prophet",
          "Independently projecting zeros and expenditure using Prophet") +
  labs(x = "Number of Training Points", y = "Absolute Error") +
  geom_hline(linetype =2, yintercept = 0)

ggsave("./figures/06_yin_yang_prophet_mean.png", device = "png", width = 8, height = 2.5, dpi = 1200)

