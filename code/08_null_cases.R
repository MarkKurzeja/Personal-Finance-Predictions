setwd("C:/Users/Mark/Dropbox/Graduate School/05) Courses/SI 671/Personal-Finance-Predictions")

source("./code/01_Data_Mugger.R")

library(prophet)
library(gridExtra)

opts_matrix <- expand.grid(begin = seq(1, 750, by = 15), end = seq(45, 800, by = 5), model = c("ARIMA", "Naive_Avg"))
opts_matrix %<>% filter(end - 30 > begin)

temp_dat <- full_dat

printed_header = T
a_ply(opts_matrix, 1, function(drow){
  model = drow$model
  begin = drow$begin
  end = drow$end
  duration = 30
  
  cat(sprintf("Begin %4i | End %4i\n", begin, end))
  
  train_dat = temp_dat[seq(begin, end), ]
  # browser()
  test_value = temp_dat$Outflow[seq(end + 1, end + duration)] %>% sum()
  
  try({
  if (model == "Naive_Avg") {
    # Compute the expected value approximation
    expected = mean(train_dat$Outflow) * 30
  } else if (model == "ARIMA") {
    # Compute the ARIMA prediction
      mmod <- arima(train_dat$Outflow, order = c(2,0,1))
      expected = sum(predict(mmod, 30)$pred %>% as.numeric())
  }
  
  write.table(x = 
                data.frame(
                  model = model, 
                  begin = begin,
                  end = end, 
                  interval_length = end - begin + 1, 
                  true_value = test_value, 
                  expected = expected, 
                  error = expected - test_value),
              row.names = F, 
              col.names = printed_header, 
              sep = ",", 
              file = "./saved_runs/08_null_cases.csv", 
              append = T)
    })
  if(printed_header) {printed_header <<- F}
}, .progress = progress_win(title = "Null Cases (08)"))
