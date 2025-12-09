make_stationary <- function(data, border, lagvar) {
  
  current_data <- data
  
  # Initialize variables based on the input data
  variables <- ncol(current_data) - 1 # Exclude DATE column
  ending <- ncol(current_data)   # Last column index     
  iteration <- 0  # To track the number of iterations                    
  ADF_pvalues <- numeric(variables) # To store ADF p-values
  names(ADF_pvalues) <- colnames(current_data)[2:ending] # Name the p-values vector
  
  repeat { 
    
    for (i in 2:ending) {
      ADF_result <- adf.test(as.numeric(current_data[[i]]))
      ADF_pvalues[i - 1] <- ADF_result$p.value # Find the P-values
      
    }
    
    # 2. Check Exit Condition
    if (iteration == 1) {
      final_data <- current_data # Save the Last Data set
      break # Stop the loop if all stationary
    }
    
    # 3. Prepare Next Iteration
    diff_data <- data.frame(DATE = current_data$DATE[-1]) # Build next Data set
    
    for (i in 1:variables) {
      name <- names(ADF_pvalues)[i]
      x <- current_data[[i + 1]]
      
      stationary <- (ADF_pvalues[i] < border)
      
      if (i <= lagvar && iteration == 0) {
        if (stationary) {
          diff_data[[name]] <- x[-1] # Keep origin -1 due to diff
        } else {
          diff_data[[name]] <- diff(log(x)) # diff log he million euros
        }
      } else {
        if (stationary) {
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