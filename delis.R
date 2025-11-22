source("libraries.R")
source("config.R")
source("stationarity_utils.R")
source("allmodels.R")
source("AutoHomo.R")

# Initialize Variables
data <- read_excel(file_path) 
variables <- ncol(data) - 1 # Find the number of the variables i have based on the Excel Format
ending <- ncol(data) # Go till the last variable <- <- in the Excel Format
current_data <- data # hold the current_data for temporary in the loop
variables <- ncol(current_data) - 1 # Find the Amount of Variables we will use for the regression -1 due to the date
ending <- ncol(current_data) # Ending Variable
y_name <- names(data)[2] # Dependent Variable Name

# Stationarity The Variables IF needed
results_list <- make_stationary(data, border, lagvar) # Def for stationarity
final_data <- results_list$final_data # Extract Final Data
final_adf_values <- results_list$ADF_pvalues # Extract ADF P-values

# Find All models
models_df <- allmodels(final_data)

# Autocorrelation, Homoscedasticity, R2, VIF, NORM, AIC tests for all models
diagnostics_df <- AutoHomo(models_df, final_data, y_name)
final_results <- cbind(models_df, diagnostics_df)

# Step 2, Correlation matrix
correlation_matrix <- cor(final_data[, 2:ending])

# Step 3 scatter-plot

# step last sort the models

sorted_results <- final_results %>%

  mutate(Is_Strictly_Valid = Autocorrelation_Pval > 0.05 & 
           Homoscedasticity_Pval > 0.05 & 
           Normality_Pval > 0.05 &   # Residuals must be normal
           VIF_Pval < 5) %>%
  
  arrange(desc(Is_Strictly_Valid), AIC_Value, desc(R_Squared)) %>%
  select(everything(), AIC_Value, Autocorrelation_Pval, R_Squared, Autocorrelation_Pval, Homoscedasticity_Pval, Is_Strictly_Valid)

head(sorted_results)

# Print Menu

print(final_adf_values) # Print the p-values
print(vif_values) # Print the multicollinearity
print(correlation_matrix) # Print the correlation
