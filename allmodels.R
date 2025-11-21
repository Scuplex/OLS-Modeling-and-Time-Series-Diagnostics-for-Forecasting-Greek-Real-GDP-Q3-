allmodels <- function(data, remove_cols = c("DATE", "Real GDP")) {
  
  # 1. Select predictors (remove DATE and Real GDP)
  predictors <- colnames(data)[ !(colnames(data) %in% remove_cols) ]
  
  # 2. Empty list to store variable sets
  all_models <- list()
  row_index <- 1
  
  # 3. Loop through model sizes 1 to number of predictors
  for (k in 1:length(predictors)) {
    
    combos <- combn(predictors, k)  # combinations of size k
    
    for (j in 1:ncol(combos)) {
      all_models[[row_index]] <- combos[, j]
      row_index <- row_index + 1
    }
  }
  
  # 4. Convert list → dataframe
  max_len <- length(predictors)
  models_df <- as.data.frame(matrix(NA, nrow = length(all_models), ncol = max_len))
  colnames(models_df) <- paste0("Var", 1:max_len)
  
  for (i in 1:length(all_models)) {
    vars <- all_models[[i]]
    models_df[i, 1:length(vars)] <- vars
  }
  
  return(models_df)
}
