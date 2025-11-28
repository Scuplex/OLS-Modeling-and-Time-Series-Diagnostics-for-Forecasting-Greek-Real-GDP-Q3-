source("libraries.R")
source("config.R")
source("stationarity_utils.R")
source("allmodels.R")
source("AutoHomo.R")

# Initialize Variables
data <- read_excel(file_path) # Read Data
y_name <- names(data)[2] # Dependent Variable Name

# 1. Separate Dummies from Data
data_for_stationarity <- data[ , !(names(data) %in% dummy_cols)] # Data without dummies
dummies_only <- data[ , dummy_cols, drop = FALSE] # Dummies only

results_list <- make_stationary(data_for_stationarity, border, lagvar)  # Def for stationarity
stationary_data <- results_list$final_data  # Stationary Data
final_adf_values <- results_list$ADF_pvalues # ADF p-values
final_kpss_values <- results_list$KPSS_pvalues # KPSS p-values

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


# Extract the best predictors
best_row <- sorted_results[1, ]
vars <- as.character(unlist(best_row[paste0("Var", 1:8)]))
vars <- vars[!is.na(vars)]
vars <- paste0("`", vars, "`")
best_model_formula <- paste("Real_GDP ~", paste(vars, collapse = " + "))
best_model_formula

X_ALL <- stationary_data[, 3:ncol(stationary_data)]
X_LAG <- dplyr::lag(X_ALL, 1)
X_LAG <- X_LAG[-1, , drop = FALSE]

Real_GDP <- stationary_data[, 2]
Real_GDP <- Real_GDP[-1]

Dates <- stationary_data[, 1]
Dates <- Dates[-1]
Final_Dataset <- data.frame(Date = Dates, Real_GDP, X_LAG, check.names = FALSE)

model <- lm(as.formula(best_model_formula), data = Final_Dataset)
summary(model)

last_row <- tail(Final_Dataset, 1)

# Predict next period
next_forecast <- predict(model, newdata = last_row)
next_forecast


