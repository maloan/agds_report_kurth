# Helper function: for a given k, fit KNN and return MAE on train + test
eval_KNN <- function(k, train_data, test_data, recipe_obj) {
  mod <- train(
    recipe_obj,
    data      = train_data,
    method    = "knn",
    tuneGrid  = data.frame(k = k),
    trControl = trainControl(method = "none"),
    metric    = "MAE"
  )
  
  # Predictions
  preds_train <- predict(mod, newdata = train_data)
  preds_test  <- predict(mod, newdata = test_data)
  
  # Compute MAE
  mae_train <- yardstick::mae_vec(truth    = train_data$GPP_NT_VUT_REF, estimate = preds_train)
  mae_test <- yardstick::mae_vec(truth    = test_data$GPP_NT_VUT_REF, estimate = preds_test)
  
  # Also compute R^2 for illustration (not strictly needed)
  r2_train <- yardstick::rsq_vec(truth    = train_data$GPP_NT_VUT_REF, estimate = preds_train)
  r2_test <- yardstick::rsq_vec(truth    = test_data$GPP_NT_VUT_REF, estimate = preds_test)
  
  tibble(
    k         = k,
    MAE_train = mae_train,
    MAE_test  = mae_test,
    R2_train  = r2_train,
    R2_test   = r2_test
  )
}
