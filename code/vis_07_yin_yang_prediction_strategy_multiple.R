setwd("C:/Users/Mark/Dropbox/Graduate School/05) Courses/SI 671/Personal-Finance-Predictions")

library(readr)
library(ggplot2)
library(plyr)
library(dplyr)


yin_yang_multiple_data <- read_csv("saved_runs/finished/07_yin_yang_prediction_multiple_results.csv")

yin_yang_multiple_data %<>% 
  filter(error < 10000, error > -10000)

yin_yang_multiple_data_sum <- yin_yang_multiple_data %>%
  group_by(model, svd, interval_length) %>%
  summarize(error = mean(error, trim = 0.01), count = n())

# Plottings
yin_yang_multiple_data_sum %>%
  ggplot() +
  facet_wrap(~model) +
  geom_point(aes(x = interval_length, y = error, color = svd), alpha = 0.3) +
  scale_color_brewer(palette = "Set1") + 
  # scale_y_continuous(limits = c(-150,400)) + 
  geom_smooth(aes(x = interval_length, y = error, color = svd)) +
  ggtitle("Estimating the Joint Distribution of Spending and Days Till Next Purchase",
          "Using SVD to remove correlation introduced bias on average") +
  labs(x = "Number of Training Points", y = "Absolute Error") +
  geom_hline(linetype =2, yintercept = 0) +
  guides(color=guide_legend(title="PCA"))

# yin_yang_multiple_data %>%
#   ggplot() +
#   facet_wrap(~model, scales = "free") +
#   geom_point(aes(x = interval_length, y = error, color = svd), alpha = 1) +
#   scale_color_brewer(palette = "Set1") + 
#   # scale_y_continuous(limits = c(-150,400)) + 
#   geom_smooth(aes(x = interval_length, y = error, color = svd)) +
#   ggtitle("Estimating the Joint Distribution of Zeros & Amounts Using Prophet",
#           "Independently projecting zeros and expenditure using Prophet") +
#   labs(x = "Number of Training Points", y = "Absolute Error") +
#   geom_hline(linetype =2, yintercept = 0)

ggsave("./figures/07_yin_yang_multiple.png", device = "png", width = 8, height = 8/16.32*7.5, dpi = 1200)

