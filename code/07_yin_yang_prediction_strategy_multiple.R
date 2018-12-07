setwd("C:/Users/Mark/Dropbox/Graduate School/05) Courses/SI 671/Personal-Finance-Predictions")

# source("./code/01_Data_Mugger.R")

library(prophet)
library(gridExtra)
library(triangle)

# This code aims to use multiple models training on a yin-yang style training - using the previous 
# amount and day lag to predict the next set




opts_matrix <- expand.grid(
  model = c(
    "GLM", 
    # "GLM_Interaction",
    # "GLM_Poly",
    # "GLM_Pois",
    "Random_Forest",
    "SVM"
  ),
  svd = c(T,F),
  begin = seq(1, 750, by = 15), 
  end = seq(45, 800, by = 5)
)
opts_matrix %<>% filter(end - 45 > begin) %>% filter(!(svd & model == "GLM_Pois"))

printed_header = T
yin_yang_results <- a_ply(opts_matrix, 1, function(drow){
  # Finding the joint distribution of the counts and the amounts for a yin-yang strategy
  begin = drow$begin
  # begin = 1
  svd = drow$svd
  end = drow$end
  # end = 60
  duration = 30
  model = drow$model
  
  cat(sprintf("%16s | Begin: %2i | End: %2i\n", model, begin, end))

  # The skips list
  if(begin == 1 & end == 260){ return("blue") }
  if(begin == 16 & end == 260){ return("blue") }
  
    
  # Need a try catch block to avoid shitshow around error handling for GAM
  try({
    
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
    
    # Account for the fact that the last value can be non-zero
    # browser()
    while(tail(counts_data$counts, 1) == -1) {
      counts_data <- counts_data[1:(nrow(counts_data) - 1), ]
    }
    
    # Create the yin-yang table that aims to predict a new y from the last y and count data and
    # a new count from old y and count data
    yin_yang <- list()
    for(i in 2:nrow(counts_data)){
      yin_yang[[i - 1]] <- data.frame(
        y = counts_data$y[i - 1],
        zeros = counts_data$counts[i - 1],
        yNew = counts_data$y[i],
        yZero = counts_data$counts[i]
      ) 
    }
    yin_yang %<>% dplyr::bind_rows()
    
    # Decouple the covariance between the values using the principal component analysis 
    # As recommended by david jungens - this is done to eliminate the cross correlation
    # component as much as possible by projecting it onto the scores matrix and then
    # running the predictions on these values. This then is rotated back into the parent frame
    # browser()
    if(svd) {
      # browser()
      p_comp_data <- princomp(yin_yang %>% dplyr::select(yNew, yZero))
      yin_yang$yNew <- p_comp_data$scores[ ,1]
      yin_yang$yZero <- p_comp_data$scores[ ,2]
    }
    
    # Run a model on it
    if(model == "GLM") {
      mmod_y <- glm(yNew ~ scale(y) + scale(zeros), data = yin_yang)
      mmod_zero <- glm(yZero ~ scale(y) + scale(zeros), data = yin_yang)
    } else if (model == "GLM_Interaction") {
      mmod_y <- glm(yNew ~ (scale(y) + scale(zeros))^2, data = yin_yang)
      mmod_zero <- glm(yZero ~ (scale(y) + scale(zeros))^2, data = yin_yang)
    } else if (model == "GLM_Poly") {
      mmod_y <- glm(yNew ~ poly(y, degree = 3) + poly(zeros, degree = 3), data = yin_yang)
      mmod_zero <- glm(yZero ~ poly(y, degree = 3) + poly(zeros, degree = 3), data = yin_yang)
    } else if (model == "GLM_Pois") {
      mmod_y <- glm(yNew ~ scale(y) + scale(zeros), data = yin_yang)
      mmod_zero <- glm(yZero ~ scale(y) + scale(zeros), data = yin_yang, family = poisson)
    } else if (model == "Random_Forest") {
      mmod_y <- randomForest::randomForest(yNew ~ y + zeros, data = yin_yang)
      mmod_zero <- randomForest::randomForest(yZero ~ y + zeros, data = yin_yang)
    } else if (model == "SVM") {
      mmod_y <- e1071::svm(yNew ~ scale(y) + scale(zeros), data = yin_yang)
      mmod_zero <- e1071::svm(yZero ~ scale(y) + scale(zeros), data = yin_yang)
    }
    
    # Run the model forward
    results <- list()
    results[[1]] <- yin_yang %>% tail(1) %>% {data.frame(y = .$yNew, zeros = .$yZero)}
    for(i in 2:10) {
      results[[i]] <- data.frame(
        y = predict(mmod_y, results[[i - 1]]),
        zeros = predict(mmod_zero, results[[i - 1]])
      )
    }
    results %<>% bind_rows()
    
    # now back out the SVD calculations
    if(svd) {
      results <- as.data.frame(as.matrix(results) %*% 
                                 t(p_comp_data$loadings) +
                                 matrix(
                                   p_comp_data$center, 
                                   nrow = nrow(results), 
                                   ncol = 2, 
                                   byrow = T)
                               )
      colnames(results) <- c("y", "zeros")
    }
    
    # Now accrue the results
    cum_sum_days <- cumsum(results$zeros) 
    # browser()
    
    # Establish quick exit for insane predictions
    if(is.nan(sum(results$zeros))) {
      return("Blue")
    }
    
    # Determine if the time frame of prediction is within 30 days or not
    if (sum(results$zeros) <= 30) {
      yhat <- sum(results$y)
    } else {
      number_to_sum = cum_sum_days %>% {which(. > 30)} %>% min() %>% {. - 1}
      yhat <- results$y[1:number_to_sum] %>% sum()
      # Weight the last days by the linear accural to account for the fact that 
      # we will never see exactly 30 days of predictions
      y_partial <- (duration - cum_sum_days[number_to_sum]) / 
        (cum_sum_days[number_to_sum + 1] - cum_sum_days[number_to_sum]) * 
        results$y[number_to_sum]
      yhat <- yhat + y_partial
    }
    
    # Now build out the data.frame and write to file
    write.table(x = 
                  data.frame(
                    model = model,
                    svd = svd, 
                    begin = begin,
                    end = end,
                    interval_length = end - begin + 1,
                    true_value = actual_value, 
                    expected = yhat, 
                    error = yhat - actual_value
                  )
                ,
                row.names = F, 
                col.names = printed_header, 
                sep = ",", 
                file = "./saved_runs/07_yin_yang_prediction_multiple_results.csv", 
                append = T)
    if(printed_header) {printed_header <<- F}
    
    
    
  }) #End of try catch
  
}, .progress = progress_win(title = "Yin Yang Multiple (07)"))
