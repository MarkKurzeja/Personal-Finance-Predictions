setwd("C:/Users/Mark/Dropbox/Graduate School/05) Courses/SI 671/Personal-Finance-Predictions")

source("./code/01_Data_Mugger.R")

library(prophet)
library(gridExtra)
library(triangle)





opts_matrix <- expand.grid(begin = seq(1, 750, by = 15), end = seq(45, 800, by = 5))
opts_matrix %<>% filter(end - 45 > begin)

printed_header = T
yin_yang_results <- a_ply(opts_matrix, 1, function(drow){
  # Finding the joint distribution of the counts and the amounts for a yin-yang strategy
  begin = drow$begin
  end = drow$end
  duration = 30
  
  cat(sprintf("Begin: %i | End: %i\n", begin, end))
    
  # Assemble the training data
  train_dat <- full_dat[seq(begin,end), ]
  actual_value <- full_dat$Outflow[seq(end + 1, end + duration)] %>% sum()
  train_counts <- data.frame(y = NULL, num_zeros = NULL)
  
  # Begin by getting a table of each amount and then the number of zeros after it until the 
  # next transaction - counts_data is going to be the dataframe that we are going to work 
  # with for the rest of the problem
  non_zero_counts <- c(which(train_dat$Outflow != 0), length(train_dat$Outflow)) %>% diff() 
  non_zero_values <- train_dat$Outflow[train_dat$Outflow != 0]
  counts_data <- data.frame(y = non_zero_values, counts = non_zero_counts - 1) %>% as_tibble()
  
  # Break into two seperate dataframes for prophet to workon
  predict_counts <- dplyr::data_frame(y = counts_data$counts, ds = seq_along(counts_data$counts) + lubridate::today())
  predict_y <- dplyr::data_frame(y = counts_data$y, ds = seq_along(counts_data$y) + lubridate::today())
  
  # Run prophet on the data
  mmod_counts <- prophet(predict_counts)
  mmod_y <- prophet(predict_y)
  
  # Get the future data
  future <- make_future_dataframe(mmod_counts, periods = duration)
  
  forecast_counts <- predict(mmod_counts, future)
  forecast_y <- predict(mmod_y, future)
  
  # Get the running count of the days
  results_counts <- tail(forecast_counts[c('yhat', 'yhat_lower', 'yhat_upper')], n = duration) 
  results_y <- tail(forecast_y[c('yhat', 'yhat_lower', 'yhat_upper')], n = duration) 
  
  results_counts[results_counts < 0] = 0 
  results_y[results_y < 0] = 0 
  
  total_sum_so_far = 0
  expectation = 0
  for(i in 1:nrow(results_counts)) {
    # Get the current day count
    # browser()
    this_day_count <- rtriangle(1, results_counts$yhat_lower[i], results_counts$yhat_upper[i],results_counts$yhat[i]) 
    # Sum the total day count and ensure that we are not over on days
    total_sum_so_far <- total_sum_so_far + this_day_count
    if (total_sum_so_far >= duration) {
      break;  
    }
    
    expectation <- expectation + rtriangle(1, results_y$yhat_lower[i], results_y$yhat_upper[i], results_y$yhat[i])
  }
  
  write.table(x = 
                data.frame(
                  begin = begin,
                  end = end,
                  interval_length = end - begin + 1,
                  true_value = actual_value, 
                  expected = expectation, 
                  error = expectation - actual_value
                  # upper = upper - actual_value, 
                  # lower = lower - actual_value
                )
              ,
              row.names = F, 
              col.names = printed_header, 
              sep = ",", 
              file = "./saved_runs/06_yin_yang_prediction_results.csv", 
              append = T)
  if(printed_header) {printed_header <<- F}
  
  
  
  # plot(mmod, forecast)
}, .progress = progress_win(title = "Prophet Window Predictions (05)"))
