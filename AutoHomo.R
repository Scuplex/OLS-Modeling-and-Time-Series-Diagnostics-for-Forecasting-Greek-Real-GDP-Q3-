source("libraries.R")

AutoHomo <- function(models_df, final_data, dep_var_name) {
  
  # 1. Create empty places to store our results
  ac_pvalues <- numeric(nrow(models_df))
  hc_pvalues <- numeric(nrow(models_df))
  r2_pvalues <- numeric(nrow(models_df))
  vif_values <- numeric(nrow(models_df))
  norm_value <- numeric(nrow(models_df))
  aikaike_value <- numeric(nrow(models_df))
  
  
  # 2. Start the loop
  for (i in 1:nrow(models_df)) {
    
    # Get the current row
    current_row <- unlist(models_df[i, ])
    predictors <- na.omit(current_row)  # Remove NAs using na.omit which returns a vector without NAs

    
    # 3. If we found variables, run the model. If not, put NA.
    if (length(predictors) > 0) {
      
      rhs_basic <- paste(paste0("`", predictors, "`"), collapse = " + ")
      f_string <- paste0("`", dep_var_name, "` ~ ", rhs_basic)
      
      # Run the Regression
      model <- lm(as.formula(f_string), data = final_data) 
      
      # Run the Tests
      ac_pvalues[i] <- bgtest(model)$p.value  # Autocorrelation 
      hc_pvalues[i] <- bptest(model)$p.value  # Homoscedasticity
      r2_pvalues[i] <- summary(model)$r.squared # R Squared
      terms_in_model <- attr(terms(model), "term.labels")
      
      if (length(terms_in_model) < 2) {
        vif_values[i] <- NA
      } else {
        vif_values[i] <- max(car::vif(model))
      }
      norm_value[i] <- ks.test(residuals(model), "pnorm", mean = mean(resid(model)), sd = sd(resid(model)))$p.value # The p-value of the Normality test must be high to accept normality p-value > 0.05
      aikaike_value[i] <- AIC(model) # AIC value for model comparison
      
    } else {
      # No variables found for this row
      ac_pvalues[i] <- NA
      hc_pvalues[i] <- NA
      r2_pvalues[i] <- NA
      vif_values[i] <- NA
      norm_value[i] <- NA
      aikaike_value[i] <- NA
    }
  } 
  
  # 4. Save and return the result
  results <- data.frame(
    Autocorrelation_Pval = ac_pvalues,
    Homoscedasticity_Pval = hc_pvalues,
    R_Squared = r2_pvalues,
    VIF_Value = vif_values,
    Normality_Pval = norm_value,
    AIC_Value = aikaike_value
  )
  
  return(results)
}