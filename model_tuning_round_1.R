tuning_dataset <- read.csv('final_dataset_for_spatio_temporal_tuning.csv')
new_tuning_data <- tuning_dataset
tuning_dataset <- tuning_dataset[, -1]
tuning_dataset <- tuning_dataset[, -3]
tuning_dataset <- tuning_dataset[, -1]
tuning_dataset <- tuning_dataset[, -17]
tuning_dataset <- tuning_dataset[, -14]
tuning_dataset <- tuning_dataset[, -4]
tuning_dataset <- tuning_dataset[, -6]
tuning_dataset <- tuning_dataset[, -7]

write.csv(tuning_dataset, 'finalized_dataframe_Apr24.csv')


new_tuning_data <- new_tuning_data[, -1]
new_tuning_data <- new_tuning_data[, -3]
new_tuning_data <- new_tuning_data[, -1]
new_tuning_data <- new_tuning_data[, -17]
new_tuning_data <- new_tuning_data[, -14]
new_tuning_data <- new_tuning_data[, -4]
new_tuning_data <- new_tuning_data[, -4]
new_tuning_data <- new_tuning_data[, -7]
new_tuning_data <- new_tuning_data[, -6]

write.csv(new_tuning_data, 'finalized_dataframe_May24.csv')

###################### creating the setup for the actual parameter tuning
updated_tuned_data <- read.csv('finalized_dataframe_May24.csv')
updated_tuned_data <- updated_tuned_data[, -1]
library(xgboost) 
library(rBayesianOptimization)
library(caret)
library(rsample) #to split stratified data
library(dplyr)
library(pROC)

total_folds <- 9
training_datasets <- list()

for (fold in 1:total_folds) {
  
  # Assuming 'fold' is a column in your dataframe that indicates the fold number for each row
  # Initialize an empty dataframe for additional_data for safety
  additional_data <- data.frame()
  
  if(fold >= 1 & fold <= 3) {
    # Folds 1-3 don't include anything
    additional_data <- additional_data
  } else if(fold >= 4 & fold <= 6) {
    # Folds 4-6 include period 20
    additional_data <- rbind(additional_data, updated_tuned_data[updated_tuned_data$period %in% c(20),])
  } else if(fold >= 7 & fold <= 9) {
    # Folds 7-9 include periods 20 and 21
    additional_data <- rbind(additional_data, updated_tuned_data[updated_tuned_data$period %in% c(20, 21),])
  }
  
  # Always include period 19
  always_include_data <- updated_tuned_data[updated_tuned_data$period == 19,]
  
  # Combine always-include data with conditionally included data
  training_data <- rbind(always_include_data, additional_data)
  
  # Store this combined training set in the list
  training_datasets[[fold]] <- training_data
}

new_lepto_incidence_performance <- data.frame(model = rep("LIP", 9),
                                              fold_id = 1:9,
                                              auc = rep(NA, 9),
                                              sensitivity = rep(NA, 9),
                                              specificity = rep(NA, 9),
                                              presence = rep(NA, 9),
                                              background = rep(NA, 9),
                                              stringsAsFactors = FALSE)


table(updated_tuned_data$fold)
for (j in 1:9){
  train <- training_datasets[[j]]
  train_labels <- train[, 10]
  train <- train[, -10]; train <- train[, -11]; train <- train[, -11]
  head(train)
  test  <- updated_tuned_data[updated_tuned_data$fold == j,]
  test_labels <- test[, 10]
  test <- test[, -10]; test <- test[, -11]; test <- test[, -11]
  head(test)
  scale_weight = round((as.data.frame(table(train_labels))[1,2] / as.data.frame(table(train_labels))[2,2]), 0)
  
  new_lepto_incidence_performance[j, "fold_id"] <- j
  new_lepto_incidence_performance[j, "presence"] <- as.data.frame(table(train_labels))[2,2]
  new_lepto_incidence_performance[j, "background"] <- as.data.frame(table(train_labels))[1,2]
  
  train <- as.matrix(train)
  training_label <- as.matrix(train_labels)
  d_train <- xgb.DMatrix(train, label=training_label)
  
  test <- as.matrix(test)
  testing_label <- as.matrix(test_labels)
  d_test <- xgb.DMatrix(test, label=test_labels)
  
  #------------------------------------------------------
  # Write function for parameter selection function as the engine of Bayesian optimization
  #------------------------------------------------------
  ntrees.max = 200
  xgb_cv_bayes <- function(eta, max.depth, min.child.weight, subsample, colsample_bytree, gamma) {
    cv <- xgb.cv(params = list(booster = "gbtree",
                               eta = eta,
                               max_depth = max.depth,
                               min_child_weight = min.child.weight,
                               subsample = subsample,
                               colsample_bytree = colsample_bytree,
                               gamma = 0,
                               objective = "binary:logistic",
                               eval_metric = "logloss",
                               seed = 25),
                 data = d_train,
                 nrounds = ntrees.max,
                 nfold = 3, 
                 early_stopping_rounds = 10,
                 scale_pos_weight = 2,
                 verbose = T)
    list(Score = -unlist(cv$evaluation_log[cv$best_iteration, "test_logloss_mean"]), # Ensure score is negative, since optimization maximizes
         Pred = cv$pred,
         cb.print.evaluation(period = 1))
  }
  
  
  #------------------------------------------------------
  # Acquire optimal parameters with Bayesian optimization (maximization function) via the R package "rBayesianOptimization"
  #------------------------------------------------------
  best_params <- BayesianOptimization(xgb_cv_bayes,
                                      bounds = list(eta = c(0.01, 0.3),
                                                    max.depth = c(2L, 10L),
                                                    min.child.weight = c(1L, 15L),
                                                    subsample = c(0.6, 1),
                                                    colsample_bytree = c(0.6, 1)),
                                      init_grid_dt = NULL,
                                      init_points = 10,
                                      n_iter = 40,
                                      acq = "ucb",
                                      kappa = 3,
                                      eps = 1.5,
                                      verbose = T)
  
  #------------------------------------------------------
  # Using the tuned hyperparameters, run a second cross-validation to acquire nrounds
  #------------------------------------------------------
  xgb_cv <- xgb.cv(params = best_params,
                   data = d_train,
                   nrounds = ntrees.max,
                   nfold = 5,
                   scale_pos_weight = scale_weight,
                   early_stopping_rounds = 10,
                   objective = "binary:logistic",
                   eval_metric = "logloss",
                   verbose = T)
  
  best_params$nrounds <- xgb_cv$best_ntreelimit
  
  #------------------------------------------------------
  # Run the full xgb model with the suite of optimal parameters
  #------------------------------------------------------
  watchlist <- list(train = d_train, test = d_test)
  xgb.fit <- xgboost(data = d_train,
                     eta = best_params$Best_Par[1],
                     max_depth = best_params$Best_Par[2],
                     min_child_weight = best_params$Best_Par[3],
                     subsample = best_params$Best_Par[4],
                     colsample_bytree = best_params$Best_Par[5],
                     gamma = 0,
                     nrounds = best_params$nrounds,
                     scale_pos_weight = scale_weight, #(330/165)
                     objective = "binary:logistic",
                     eval_metric = "logloss")
  
  ###prediction test
  xgbpred <- predict(xgb.fit, d_test)
  xgbpred <- as.numeric(xgbpred)
  labels <- as.numeric(test_labels)
  
  auc <- roc(response=labels, predictor=xgbpred, levels=c(0,1))
  new_lepto_incidence_performance[j, "auc"] <- auc$auc
  
  threshold <- pROC::coords(auc, "best", ret = "threshold")
  CM <- confusionMatrix(as.factor(ifelse(xgbpred >= threshold$threshold, 1, 0)), as.factor(labels), positive = "1")
  CM_table <- as.data.frame(CM[[4]])
  new_lepto_incidence_performance[j, "sensitivity"] <- CM_table[1,]
  new_lepto_incidence_performance[j, "specificity"] <- CM_table[2,]
}

write.csv(new_lepto_incidence_performance, "new_variables_parameter_tuned_model_performance_13_22.csv")

mean_auc <- mean(new_lepto_incidence_performance$auc)
auc_sd <- sd(new_lepto_incidence_performance$auc)
auc_se <- auc_sd/sqrt(9)
alpha = 0.05

t_score <- qt(p = alpha/2, df = 8, lower.tail = F)

margin_error <- t_score * auc_se
auc_lower_bound <- mean_auc - margin_error
auc_upper_bound <- mean_auc + margin_error


# when doing incidence, change from binary logistic to something else
# for objective, try tweedie, gamma, and poisson distributions and see which performs the best


# how to deal with ones and 0s issue- run with everything naturally
# once you train the model with 80% of the data, and test with the 20% 
# move to nrmse again for spatio-temporal crossvalidation but hold on and also plot predict versus actual for each fold
# store predicted and actual value and then plot them - with fold value associated

# then calculate nrmse for non-zero values and 0 values separate from each other
# note this needs the best parameters thing


# additional thing to do - throw out all the zeros and give it the non-zero values
# maybe later
