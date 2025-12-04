source("libraries.R")
source("config.R")
source("stationarity_utils.R")
source("allmodels.R")
source("AutoHomo.R")
source("models_estimate.R")

# Initialize Variables
data <- read_excel(file_path) # Read Data
y_name <- names(data)[2] # Dependent Variable Name

# 1. Separate Dummies from Data
data_for_stationarity <- data[ , !(names(data) %in% dummy_cols)] # Data without dummies
dummies_only <- data[ , dummy_cols, drop = FALSE] # Dummies only

results_list <- make_stationary(data_for_stationarity, border, lagvar)  # Def for stationarity
stationary_nodum <- results_list$final_data  # Stationary Data
final_adf_values <- results_list$ADF_pvalues # ADF p-values 
#final_kpss_values <- results_list$KPSS_pvalues # KPSS p-values

rows_lost <- nrow(data) - nrow(stationary_nodum) # find how many rows we lost due to stationarity
dummies_trimmed <- tail(dummies_only, -rows_lost) # Trim the dummies to match the stationary data

stationary_dum <- cbind(stationary_nodum, dummies_trimmed) # Combine Stationary Data with Dummies

# Lagged Dataset
X_ALL <- stationary_nodum[1:62, 3:ncol(stationary_nodum)]
X_LAG <- dplyr::lag(X_ALL, 1)
X_LAG <- X_LAG[-1, , drop = FALSE]

Real_GDP <- stationary_nodum[1:62, 2]
Real_GDP <- Real_GDP[-1]

Dates <- stationary_nodum[1:62, 1]
Dates <- Dates[-1]
Temp_Dataset <- data.frame(Date = Dates, "Real GDP" = Real_GDP, X_LAG, check.names = FALSE)

dummies_trimmed <- dummies_trimmed[-1, ] # Trim the dummies to match the lagged data
Lagged_Dataset <- cbind(Temp_Dataset, dummies_trimmed[1:nrow(Temp_Dataset), , drop = FALSE])

# Find All models
models_df <- allmodels(stationary_nodum) # Find all models with a loop

# Autocorrelation, Homoscedasticity, R2, VIF, NORM, AIC tests for all models
diagnostics_df <- AutoHomo(models_df, Lagged_Dataset, y_name)
final_results <- cbind(models_df, diagnostics_df) # Combine Models with Diagnostics

# Correlation matrix
correlation_matrix <- cor(Lagged_Dataset[, 2:(ncol(data)-2)]) # Exclude DATE column

# Step last sort the models

sorted_results <- final_results %>%

  mutate(Is_Strictly_Valid = Autocorrelation_Pval > border & 
           Homoscedasticity_Pval > border & 
           Normality_Pval > border &   # Residuals must be normal
           VIF_Value < 5) %>%
  
  arrange(desc(Is_Strictly_Valid), AIC_Value, desc(R_Squared)) %>%
  select(-any_of(dummy_cols)) %>%
  select(everything(), AIC_Value, Autocorrelation_Pval, R_Squared, Homoscedasticity_Pval, Is_Strictly_Valid)


# estimate each model their pvalues f statistic exc
valid_models <- sorted_results[sorted_results$Is_Strictly_Valid == TRUE, 1:8] # get only valid models
Model_Estimate <- ModelEstimate(valid_models, Lagged_Dataset, y_name)
final_models_est <- cbind(valid_models,Model_Estimate)



# # # Extract the best predictors
# best_row <- sorted_results[1, ]
# vars <- as.character(unlist(best_row[paste0("Var", 1:8)]))
# vars <- vars[!is.na(vars)]
# vars <- paste0("`", vars, "`")
# best_model_formula <- paste("Real_GDP ~", paste(vars, collapse = " + "))
# best_model_formula
# 
# model <- lm(as.formula(best_model_formula), data = Lagged_Dataset)
# summary(model)
# coefs <- coef(model)

# forecast yt+1
Intercept <- coefs[1]
x_european <- Lagged_Dataset[66, 8]
Y_future <- -0.002142492 + 0.005167519  * x_european

