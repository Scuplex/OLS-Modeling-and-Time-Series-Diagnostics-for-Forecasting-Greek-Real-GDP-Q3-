source("libraries.R")
source("config.R")

MaeMspe <- function(valid_models, Lagged_Dataset, dep_var_name, Predict_Dataset) {
  
  # 1. Create empty places to store our results
  MSPE <- numeric(nrow(valid_models))
  MAE <- numeric(nrow(valid_models)) # For the F-statistic check
  fixed_dummies <- " + `Dcovid`"
  
  # 2. Start the loop
  for (i in 1:nrow(valid_models)) {
    
    # Get the current row
    current_row <- unlist(valid_models[i, ])
    predictors <- na.omit(current_row)  # Remove NAs using na.omit which returns a vector without NAs
    
    
    # 3. If we found variables, run the model. If not, put NA.
    if (length(predictors) > 0) {
      
      rhs_basic <- paste(paste0("`", predictors, "`"), collapse = " + ")
      rhs <- paste0(rhs_basic, fixed_dummies)
      f_string <- paste0("`", dep_var_name, "` ~ ", rhs)
      
      # Run the Regression
      model <- lm(as.formula(f_string), data = Lagged_Dataset) 
      predictions_oos <- predict(model, newdata = Predict_Dataset)
      
      comparison <- data.frame(
        Date = Predict_Dataset$Date,
        Actual_GDP = Predict_Dataset$`Real GDP`,
        Predicted_GDP = predictions_oos
      )
      
      residuals <- comparison$Actual_GDP - comparison$Predicted_GDP
      MSPE[i] <- mean(residuals^2)
      MAE[i] <- mean(abs(residuals))
      
    } else {
      MSPE[i] <- NA
      MAE[i] <- NA
    }
  } 
  
  # 4. Save and return the result
  results <- data.frame(
    MSPE_val = MSPE,
    MAE_val  = MAE
  )
  
  return(results)
}