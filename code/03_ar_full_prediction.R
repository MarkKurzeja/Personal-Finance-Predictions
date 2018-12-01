setwd("C:/Users/Mark/Dropbox/Graduate School/05) Courses/SI 671/Personal-Finance-Predictions")

source("./code/01_Data_Mugger.R")

library(e1071)

begin = 1
sizemonth = 30

opts_matrix <- expand.grid(
  model = c("GLM", 
            "GEE_AR1", 
            "GEE_exch", 
            "GEE_indep", 
            "GLM_Smooth_Spline_3", "GLM_Smooth_Spline_8", 
            "GLM_Interaction", "SVM_Linear", "SVM_Radial", 
            "SVM_polynomial"),
  end = seq(75, nrow(full_dat) - sizemonth, by = 10),
  lag = seq(5, 30, by = 5)
  )

ar_full_prediction_results <- adply(opts_matrix, 1, function(drow) {
  
  lag = drow$lag
  end = drow$end
  model = drow$model
  dat <- ar_data_full[[lag]]
  # browser()
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
  } else if (model == "SVM_Linear") {
    mod <- e1071::svm(y ~ ., data = training_data, kernel = "linear")
  } else if (model == "SVM_Radial") {
    mod <- e1071::svm(y ~ ., data = training_data, kernel = "radial")
  } else if (model == "SVM_polynomial") {
    mod <- e1071::svm(y ~ ., data = training_data, kernel = "polynomial")
  } else if (model == "GLM_Smooth_Spline_3") {
    sprintf("Run 3 | %i\n",end) %>% print
    if(end < 150) {
      return(data.frame(model = NA, lag = NA, error = NA, true = NA, expected = NA))
    }
    mod <- glm(y ~ . +
                 splines::bs(lag.1, df = 3) +
                 splines::bs(lag.2, df = 2) +
                 splines::bs(lag.3, df = 2),
               data = training_data)  
  } else if (model == "GLM_Smooth_Spline_8") {
    sprintf("Run 8 | %i\n",nrow(training_data)) %>% print()
    if(end < 225) {
      return(data.frame(model = NA, lag = NA, error = NA, true = NA, expected = NA))
    }
    mod <- glm(y ~ . +
                 splines::bs(lag.1, df = 8) +
                 splines::bs(lag.2, df = 5) +
                 splines::bs(lag.3, df = 3),
               data = training_data)  
  } else if (model == "GEE_AR1") {
    mod <- geepack::geeglm(y ~ ., 
                           id = sort(rep(1:30, length = nrow(training_data))),
                           data = training_data,
                           corstr = "ar1")
  } else if (model == "GEE_exch") {
    mod <- geepack::geeglm(y ~ ., 
                           id = sort(rep(1:30, length = nrow(training_data))),
                           data = training_data,
                           corstr = "exchangeable")
  } else if (model == "GEE_indep") {
    mod <- geepack::geeglm(y ~ ., 
                           id = sort(rep(1:30, length = nrow(training_data))),
                           data = training_data,
                           corstr = "independence")
  } 
  
  # Run the model forward
  for (t in seq(end + 1, end + sizemonth)) {
    # Predict the last value using lags
    size_of_input = length(input_data)
    test_dat <- data.frame(lag = t(rev(input_data[seq(size_of_input - lag + 1, size_of_input)])))
    new_val = as.numeric(predict(mod, test_dat))
    input_data <- append(input_data, new_val)
  }
  
  final_result <- sum(input_data[seq(end + 1, end + sizemonth)])
  error = final_result - output_value
  
  data.frame(model = model, lag = lag, error = error, true = output_value, expected = final_result)
}, .progress = progress_win()) %>% filter(!is.na(model))

# Preprocess the data setting the upper and lower bounds and removing extreamly high and extreme low
# values
ar_full_prediction_results %<>% filter(!is.na(expected))
mmedian <- function(x, low = -1000, high = 1000) {median(c(low,high,x))}; mmedian <- Vectorize(mmedian)
ar_full_prediction_results$expected %<>% mmedian
ar_full_prediction_results$error %<>% mmedian

# Plottings
ar_full_prediction_results %>% 
  ggplot() +
  # scale_y_continuous(limits = c(-1000,1000)) + 
  geom_line(aes(x = end, y = error, color = factor(lag), group = lag)) +
  facet_wrap(~model, scales = "free_y") +
  geom_smooth(aes(x = end, y = error)) + 
  ggtitle("Model Errors Using Autoregressive Methods and Recursive Computation", 
          "This analysis included zeros into the lag terms") + 
  labs(x = "Number of Training Points", y = "Absolute Error") +
  geom_hline(linetype =2, yintercept = 0)

ggsave("./figures/ar_errors_w_zeros.pdf", device = "pdf", width = 10, height = 5, dpi = 500)

ar_full_prediction_results %>% 
  ggplot() + 
  geom_point(aes(x = true, y = expected)) +
  # geom_point(aes(x = expected, y = 1))  
  facet_wrap(~model, scales = "free") + 
  geom_abline(slope = 1, intercept = 0, color = "black") + 
  geom_smooth(aes(x = true, y = expected)) +
  ggtitle("Model Real Vs Expected Values Using Autoregressive Methods and Recursive Computation", 
          "This analysis included zeros into the lag terms")

ggsave("./figures/ar_truevexpected_w_zeros.png", device = "png", width = 10, height = 5)
