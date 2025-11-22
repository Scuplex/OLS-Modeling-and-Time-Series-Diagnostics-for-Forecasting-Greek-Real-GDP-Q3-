make_stationary <- function(data, border, lagvar) {
  
  current_data <- data
  # Initialize variables based on the input data
  variables <- ncol(current_data) - 1 
  ending <- ncol(current_data)        
  iteration <- 0                      
  ADF_pvalues <- numeric(variables) 
  names(ADF_pvalues) <- colnames(current_data)[2:ending]
  
  
  # Your exact loop logic
  repeat { 
    
    for (i in 2:ending) {
      ADF_result <- adf.test(as.numeric(current_data[[i]]))
      ADF_pvalues[i - 1] <- ADF_result$p.value # Find the P-values
    }
    
    # 2. Check Exit Condition
    if (iteration == 2) {
      final_data <- current_data # Save the Last Dataset
      break # Stop the loop if all stationary
    }
    
    # 3. Prepare Next Iteration
    diff_data <- data.frame(DATE = current_data$DATE[-1]) # Build next dataset
    
    for (i in 1:variables) {
      name <- names(ADF_pvalues)[i]
      x <- current_data[[i + 1]]
      
      if (i <= lagvar && iteration == 0) {# && iteration == 0 BECAUSE after the first one the percentages MUST be diff not per changed
        if (ADF_pvalues[i] < border) {
          diff_data[[name]] <- x[-1] # KEEP ORIGINAL VALUES BUT REMOVE FIRST ROW
        } else {
          diff_data[[name]] <- diff(log(x)) # PERCENTAGE CHANGE
        }
      } else {
        if (ADF_pvalues[i] < border) {
          diff_data[[name]] <- x[-1] # KEEP ORIGINAL VALUES BUT REMOVE FIRST value
        } else {
          diff_data[[name]] <- diff(x) # DIFFERENCE
        }
      }
    }
    current_data <- diff_data # Change the current_data to the diff_data for the next loop
    iteration <- iteration + 1   # Move to next iteration
  }
  
  return(list(final_data = final_data, ADF_pvalues = ADF_pvalues))
}