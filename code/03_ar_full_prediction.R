setwd("C:/Users/Mark/Dropbox/Graduate School/05) Courses/SI 671/Personal-Finance-Predictions")

source("./code/01_Data_Mugger.R")

library(e1071)
library(gbm)

sizemonth = 30

opts_matrix <- expand.grid(
  model = c(
    "GLM", 
    # "GEE_AR1", 
    # "GEE_exch", 
    # "GEE_indep", 
    # "GBM", 
    "Random_Forest", 
    "Decision_Tree", 
    # "GLM_Interaction", 
    "SVM_Linear", 
    "SVM_Radial", 
    "SVM_polynomial"
  ),
  begin = seq(1, 750, by = 25), 
  end = seq(45, 800, by = 10),
  lag = c(20)
)
opts_matrix %<>% filter(end - lag - 15 > begin)

printed_header = T
a_ply(opts_matrix, 1, function(drow) {
  
  begin = drow$begin
  lag = drow$lag
  end = drow$end
  model = drow$model
  dat <- ar_data_full[[lag]]
  
  # Get the training data for this set as well as the sum we are 
  # looking for
  training_data = dat[seq(1, end - lag), ]
  input_data = full_dat$Outflow[seq(begin, end)]
  output_value = sum(full_dat$Outflow[seq(end + 1, end + sizemonth)])
  
  # Pick a model
  mod <- NA
  if(model == "GLM_Interaction") {
    mod <- glm(y ~ (lag.1 + lag.2 + lag.3)^2 + ., data = training_data)
  } else if (model == "GLM") {
    mod <- glm(y ~ ., data = training_data)
  # } else if (model == "GLM_Gamma") {
  #   mod <- glm(y ~ ., data = training_data, family = Gamma)
  } else if (model == "SVM_Linear") {
    mod <- e1071::svm(y ~ ., data = training_data, kernel = "linear")
  } else if (model == "SVM_Radial") {
    mod <- e1071::svm(y ~ ., data = training_data, kernel = "radial")
  } else if (model == "SVM_polynomial") {
    mod <- e1071::svm(y ~ ., data = training_data, kernel = "polynomial")
  # } else if (model == "GBM") {
    # if (end - begin < 100) {
      # return("blue")
    # }
    # print("Hit...")
    # mod <- gbm::gbm(y ~ ., data = training_data, distribution = "gaussian", n.trees = 100)
  } else if (model == "Random_Forest") {
    mod <- randomForest::randomForest(y ~ ., data = training_data)
  } else if (model == "Decision_Tree") {
    mod <- rpart::rpart(y ~ ., data = training_data)
  # } else if (model == "GEE_AR1") {
  #   mod <- geepack::geeglm(
  #     y ~ ., 
  #     id = sort(rep(1:30, length = nrow(training_data))),
  #     data = training_data,
  #     corstr = "ar1"
  #   )
  } else if (model == "GEE_exch") {
    mod <- geepack::geeglm(
      y ~ ., 
      id = sort(rep(1:30, length = nrow(training_data))),
      data = training_data,
      corstr = "exchangeable"
    )
  # } else if (model == "GEE_indep") {
  #   mod <- geepack::geeglm(
  #     y ~ ., 
  #     id = sort(rep(1:30, length = nrow(training_data))),
  #     data = training_data,
  #     corstr = "independence"
  #   )
  } 
  
  # Run the model forward
  for (t in seq(end + 1, end + sizemonth)) {
    # Predict the last value using lags
    size_of_input = length(input_data)
    test_dat <- data.frame(lag = t(rev(input_data[seq(size_of_input - lag + 1, size_of_input)])))
    new_val = as.numeric(predict(mod, test_dat))
    input_data <- append(input_data, new_val)
  }
  
  final_result <- sum(input_data %>% tail(sizemonth))
  error = final_result - output_value
  
  write.table(x = 
                data.frame(
                  begin = begin, 
                  end = end, 
                  interval_length = end - begin + 1, 
                  model = model, lag = lag, 
                  error = round(error, 2), 
                  true = round(output_value,2), 
                  expected = round(final_result,2)
                ),
              row.names = F, 
              col.names = printed_header, 
              sep = ",", 
              file = "./saved_runs/03_ar_full_prediction_results.csv", 
              append = T)
  if(printed_header) {printed_header <<- F}
  
}, .progress = progress_win("Running 03 File"))




