allmodels <- function(data, remove_cols = c("DATE", "Real GDP")) {
  
  # 1. Select predictors
  predictors <- colnames(data)[ !(colnames(data) %in% remove_cols) ] # all columns except DATE and Real GDP
  all_models <- list() # to store all combinations
  row_index <- 1 # to track position in all_models
  
  # 2. Find All Models
  for (k in 1:length(predictors)) {
    
    combos <- combn(predictors, k)  # function to get combinations
    
    for (j in 1:ncol(combos)) {
      all_models[[row_index]] <- combos[, j]
      row_index <- row_index + 1
    }
  }
  
  # 3. Save them into a Dataframe
  max_len <- length(predictors)
  models_df <- as.data.frame(matrix(NA, nrow = length(all_models), ncol = max_len)) # create a Dataframe with NAs
  colnames(models_df) <- paste0("Var", 1:max_len) # give names to columns
  
  for (i in 1:length(all_models)) {
    vars <- all_models[[i]] # grab the team of variables
    models_df[i, 1:length(vars)] <- vars # fill in the variables from left to right
  }
  
  return(models_df)
}
