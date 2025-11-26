source("libraries.R")
source("config.R")
source("stationarity_utils.R")
source("allmodels.R")
source("AutoHomo.R")

# Initialize Variables
data <- read_excel(file_path) 
y_name <- names(data)[2] # Dependent Variable Name

# 1. Separate Dummies from Data
data_for_stationarity <- data[ , !(names(data) %in% dummy_cols)] # Data without dummies
dummies_only <- data[ , dummy_cols, drop = FALSE] # Dummies only

results_list <- make_stationary(data_for_stationarity, border, lagvar)  # Def for stationarity
stationary_data <- results_list$final_data  # Stationary Data
final_adf_values <- results_list$ADF_pvalues # ADF p-values

rows_lost <- nrow(data) - nrow(stationary_data) # find how many rows we lost due to stationarity
dummies_trimmed <- tail(dummies_only, -rows_lost) # Trim the dummies to match the stationary data

final_data <- cbind(stationary_data, dummies_trimmed) # Combine Stationary Data with Dummies

# Find All models
models_df <- allmodels(stationary_data) # Find all models with a loop

# Autocorrelation, Homoscedasticity, R2, VIF, NORM, AIC tests for all models
diagnostics_df <- AutoHomo(models_df, final_data, y_name)
final_results <- cbind(models_df, diagnostics_df) # Combine Models with Diagnostics

# Correlation matrix
correlation_matrix <- cor(final_data[, 2:(ncol(data)-2)]) # Exclude DATE column

# Step last sort the models

sorted_results <- final_results %>%

  mutate(Is_Strictly_Valid = Autocorrelation_Pval > border & 
           Homoscedasticity_Pval > border & 
           Normality_Pval > border &   # Residuals must be normal
           VIF_Value < 5) %>%
  
  arrange(desc(Is_Strictly_Valid), AIC_Value, desc(R_Squared)) %>%
  select(-any_of(dummy_cols)) %>%
  select(everything(), AIC_Value, Autocorrelation_Pval, R_Squared, Homoscedasticity_Pval, Is_Strictly_Valid)

head(sorted_results)