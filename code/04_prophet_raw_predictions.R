setwd("C:/Users/Mark/Dropbox/Graduate School/05) Courses/SI 671/Personal-Finance-Predictions")

source("./code/01_Data_Mugger.R")

library(prophet)
library(gridExtra)


opts_matrix <- expand.grid(end = seq(45, 800, by = 5))
temp_dat <- full_dat
colnames(temp_dat) <- c("ds", "y")

prophet_ar_wzeros_results <- adply(opts_matrix, 1, function(drow){
  begin = 1
  end = drow$end
  duration = 30
  
  # browser()
  train_dat = temp_dat[seq(begin, end), ]
  test_value = temp_dat$y[seq(end + 1, end + duration)] %>% sum()
  
  mmod <- prophet(train_dat)
  
  future <- make_future_dataframe(mmod, periods = duration)
  
  forecast <- predict(mmod, future)
  out_val <- tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')], n = duration) %>% .$yhat %>% sum()
  data.frame(true = test_value, expected = out_val, error = out_val - test_value)
  # plot(mmod, forecast)
}, .progress = progress_win())

p1 <- prophet_ar_wzeros_results %>%
  ggplot() + 
  geom_line(aes(x = end, y = error)) + 
  geom_hline(linetype = 2, yintercept = 0) + 
  # geom_smooth(aes(x = end, y = error)) +
  ggtitle("Facebook Prophet Predictions on Raw Time Series", 
          "This method is predicting on the zero-inflated time series") +
  labs(x = "Number of Training Points", y = "Absolute Error")


p2 <- prophet_ar_wzeros_results %>% 
  ggplot() + 
  geom_point(aes(x = true, y = expected)) +
  geom_abline(slope = 1, intercept = 0, color = "blue") + 
  ggtitle("Facebook Prophet Expected Vs True Distribution ") + 
  labs(x = "Actual Value", y = "Predicted Value")


ggsave(plot = grid.arrange(p1,p2), filename = "./figures/prophet_errors_w_zeros.pdf", device = "pdf", width = 10, height = 5, dpi = 500)
