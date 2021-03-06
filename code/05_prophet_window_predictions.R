setwd("C:/Users/Mark/Dropbox/Graduate School/05) Courses/SI 671/Personal-Finance-Predictions")

source("./code/01_Data_Mugger.R")

library(prophet)
library(gridExtra)


opts_matrix <- expand.grid(begin = seq(1, 750, by = 15), end = seq(45, 800, by = 5))
opts_matrix %<>% filter(end > begin)
temp_dat <- window_dat
colnames(temp_dat) <- c("ds", "y")

printed_header = T
prophet_ar_window_results <- a_ply(opts_matrix, 1, function(drow){
  begin = drow$begin
  end = drow$end
  duration = 30
  
  # browser()
  train_dat = temp_dat[seq(begin, end), ]
  test_value = temp_dat$y[end + duration] 
  
  mmod <- prophet(train_dat)
  
  future <- make_future_dataframe(mmod, periods = duration)
  
  forecast <- predict(mmod, future)
  out_val <- tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')], n = 1) %>% .$yhat
  upper <- tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')], n = 1) %>% .$yhat_upper
  lower <- tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')], n = 1) %>% .$yhat_lower
  
  write.table(x = 
                data.frame(
                  begin = begin,
                  end = end,
                  interval_length = end - begin + 1,
                  true_value = test_value, 
                  expected = out_val, 
                  error = out_val - test_value, 
                  upper = upper - test_value, 
                  lower = lower - test_value
                  )
              ,
              row.names = F, 
              col.names = printed_header, 
              sep = ",", 
              file = "./saved_runs/05_prophet_results_window.csv", 
              append = T)
  if(printed_header) {printed_header <<- F}
  
  # plot(mmod, forecast)
}, .progress = progress_win(title = "Prophet Window Predictions (05)"))

# p1 <- prophet_ar_window_results %>%
#   ggplot() + 
#   geom_ribbon(aes(x = end, ymin = lower, ymax = upper), alpha = 0.1, fill = "blue") +
#   geom_line(aes(x = end, y = error)) + 
#   geom_hline(linetype = 2, yintercept = 0) + 
#   # geom_smooth(aes(x = end, y = error)) +
#   ggtitle("Facebook Prophet Predictions on Rolling Time Series", 
#           "This method is predicting on the window summed time series") +
#   labs(x = "Number of Training Points", y = "Absolute Error")
# 
# 
# p2 <- prophet_ar_window_results %>% 
#   ggplot() + 
#   geom_point(aes(x = true, y = expected)) +
#   geom_abline(slope = 1, intercept = 0, color = "blue") + 
#   ggtitle("Facebook Prophet Expected Vs True Distribution ") + 
#   labs(x = "Actual Value", y = "Predicted Value")
# 
# 
# ggsave(plot = grid.arrange(p1,p2), filename = "./figures/prophet_errors_window_runs.pdf", device = "pdf", width = 10, height = 5, dpi = 500)
# # ggsave("./figures/prophet_truevexpected_w_zeros.pdf", device = "pdf", width = 10, height = 5, dpi = 500)
