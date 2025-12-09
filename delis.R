source("libraries.R")
source("config.R")
source("stationarity_utils.R")
source("allmodels.R")
source("AutoHomo.R")
source("models_estimate.R")
source("MAE_MSPE.R")

# Initialize Variables
data <- read_excel(file_path) # Read Data
y_name <- names(data)[2] # Dependent Variable Name

# 1. Separate Dummies from Data
data_for_stationarity <- data[ , !(names(data) %in% dummy_cols)] # Data without dummies
dummies_only <- data[ , dummy_cols, drop = FALSE] # Dummies only

results_list <- make_stationary(data_for_stationarity, border, lagvar)  # Def for stationarity
stationary_nodum <- results_list$final_data  # Stationary Data
final_adf_values <- results_list$ADF_pvalues # ADF p-values 
rows_lost <- nrow(data) - nrow(stationary_nodum) # find how many rows we lost due to stationarity
dummies_trimmed <- tail(dummies_only, -rows_lost) # Trim the dummies to match the stationary data
stationary_dum <- cbind(stationary_nodum, dummies_trimmed) # Combine Stationary Data with Dummies

# Train-Predicting Dataset
X_ALL <- stationary_nodum[, 3:ncol(stationary_nodum)]
Y_ALL <- stationary_nodum[, 2]
X_LAGGED <- dplyr::lag(X_ALL, 1)
Full_Lagged_Data <- data.frame(
  Date = stationary_nodum[, 1],
  `Real GDP` = Y_ALL,
  X_LAGGED,
  check.names = FALSE
)
Full_Lagged_Data <- cbind(Full_Lagged_Data, dummies_trimmed)
Full_Lagged_Data <- Full_Lagged_Data[-1, ]

n_train_new <- 61
Train_Dataset <- Full_Lagged_Data[1:n_train_new, ]
Predict_Dataset <- Full_Lagged_Data[(n_train_new + 1):nrow(Full_Lagged_Data), ]


# Find All models
models_df <- allmodels(stationary_nodum) # Find all models with a loop

# Autocorrelation, Homoscedasticity, R2, VIF, NORM, AIC tests for all models
diagnostics_df <- AutoHomo(models_df, Train_Dataset, y_name)
final_results <- cbind(models_df, diagnostics_df) # Combine Models with Diagnostics

# Correlation matrix
correlation_matrix <- cor(Full_Lagged_Data[, 2:(ncol(Full_Lagged_Data)-2)]) # Exclude DATE column
image(correlation_matrix, col = colorRampPalette(c("red", "green"))(100), axes = FALSE, main = "Correlation Matrix Heatmap")
axis(1, at = seq(0, 1, length.out = ncol(correlation_matrix)), labels = colnames(correlation_matrix), las = 2)
axis(2, at = seq(0, 1, length.out = nrow(correlation_matrix)), labels = colnames(correlation_matrix), las = 2)

# Step last sort the models

sorted_results <- final_results %>%

  mutate(Is_Strictly_Valid = Autocorrelation_Pval > border & 
           Homoscedasticity_Pval > border & 
           Normality_Pval > border &   # Residuals must be normal
           VIF_Value < 5) %>%
  
  arrange(desc(Is_Strictly_Valid), AIC_Value, desc(R_Squared)) %>%
  select(-any_of(dummy_cols)) %>%
  select(everything(), AIC_Value, Autocorrelation_Pval, R_Squared, Homoscedasticity_Pval, Is_Strictly_Valid)


# Estimate each model their p-values f statistic etc
valid_models <- sorted_results[sorted_results$Is_Strictly_Valid == TRUE, 1:variabless] # get only valid models
Model_Estimate <- ModelEstimate(valid_models, Train_Dataset, y_name)
final_models_est <- cbind(valid_models,Model_Estimate) # find the r2 of all models and the p-values

# Estimate the MAE and the MSPE of each model

MAE_MSPE <- MaeMspe(valid_models, Train_Dataset, y_name, Predict_Dataset)
Models_ending <- cbind(final_models_est,MAE_MSPE)

best_models <- Models_ending %>%
  arrange(MSPE_val, MAE_val)

# Pick 1 model

model_1 <- lm(`Real GDP` ~ `Receipt travels` + `EXPORT GOOD/SER` + `CPI QQ` + `Employment Rate` + `Dcovid`, 
              data = Train_Dataset)
model_1 <- lm(`Real GDP` ~ `European GDP` + `CPI QQ` + `German Searches` + `Employment Rate` + `Dcovid`, 
              data = Train_Dataset)

summary(model_1)

predictions_oos <- predict(model_1, newdata = Predict_Dataset)

comparison <- data.frame(
  Date = Predict_Dataset$Date,
  Actual_GDP = Predict_Dataset$`Real GDP`,
  Predicted_GDP = predictions_oos
)

residuals <- comparison$Actual_GDP - comparison$Predicted_GDP
MSPE <- mean(residuals^2)
MAE <- mean(abs(residuals))

ggplot(comparison, aes(x = Date)) +
  geom_line(aes(y = Actual_GDP, color = "Actual"), linewidth = 1.2) +
  geom_line(aes(y = Predicted_GDP, color = "Predicted"), linewidth = 1.2, linetype = "dashed") +
  scale_color_manual(values = c("Actual" = "black", "Predicted" = "red")) +
  labs(title = "Final Forecast Validation", y = "Real GDP", x = "Date") +
  theme_minimal()