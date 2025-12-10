source("libraries.R")
source("config.R")

ModelEstimate <- function(valid_models, Lagged_Dataset, dep_var_name) {
  
  # 1. Create empty places to store our results
  adj_r_squared <- numeric(nrow(valid_models))
  model_p_value <- numeric(nrow(valid_models)) # For the F-statistic check
  fixed_dummies <- " + `Dcovid` + `Drebound`"
  
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
      model_sum <- summary(model)
      
      # Run the Tests
      adj_r_squared[i] <- model_sum$adj.r.squared  # Autocorrelation 
      f_stat <- model_sum$fstatistic
      if (!is.null(f_stat)) {
        model_p_value[i] <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
      } else {
        model_p_value[i] <- NA
      }
      
    } else {
      # No variables found for this row
      adj_r_squared[i] <- NA
      model_p_value[i] <- NA
    }
  } 
  
  # 4. Save and return the result
  results <- data.frame(
    Adjusted_R2 = adj_r_squared,
    Model_Fpval  = model_p_value
  )
  
  return(results)
}