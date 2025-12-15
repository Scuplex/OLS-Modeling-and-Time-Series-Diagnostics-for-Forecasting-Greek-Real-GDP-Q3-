source("libraries.R")
source("config.R")
source("stationarity_utils.R")
source("allmodels.R")
source("AutoHomo.R")
source("models_estimate.R")
source("MAE_MSPE.R")

# Initialize Data set

data <- read_excel(file_path) # Read Data
y_name <- names(data)[2] # Dependent Variable Name

# 1. Stationarity Check / Stationary data

data_for_stationarity <- data[ , !(names(data) %in% dummy_cols)] # Data without dummies
dummies_only <- data[ , dummy_cols, drop = FALSE] # Dummies only
results_list <- make_stationary(data_for_stationarity, border, lagvar)  # Def for stationarity
stationary_nodum <- results_list$final_data  # Stationary Data
final_adf_values <- results_list$ADF_pvalues # ADF p-values for each variable
rows_lost <- nrow(data) - nrow(stationary_nodum) # find how many rows we lost due to stationarity
dummies_trimmed <- tail(dummies_only, -rows_lost) # Trim the dummies to match the stationary data
stationary_dum <- cbind(stationary_nodum, dummies_trimmed) # Combine Stationary Data with Dummies

# 2. Lagged Data set / Train - Predict Data set

X_ALL <- stationary_nodum[, 3:ncol(stationary_nodum)] # get all X
Y_ALL <- stationary_nodum[, 2] # get all Y
X_LAGGED <- dplyr::lag(X_ALL, 1) # Lag X
Y_LAGGED <- dplyr::lag(Y_ALL, 1) # Lag Y
Full_Lagged_Data <- data.frame( Date = stationary_nodum[, 1], `Real GDP` = Y_ALL, X_LAGGED,Y_LAGGED, check.names = FALSE) # Create a data frame combining them
mod_data <- Full_Lagged_Data[-1] # Data set for models WITHOUT dummies
Full_Lagged_Data <- cbind(Full_Lagged_Data, dummies_trimmed)
Full_Lagged_Data <- Full_Lagged_Data[-1, ] # Lagged Data set for models

# Train-Predicting Data set
Train_Dataset <- Full_Lagged_Data[1:n_train_new, ]
Predict_Dataset <- Full_Lagged_Data[(n_train_new + 1):nrow(Full_Lagged_Data), ]

# 3. Find All models - Autocorrelation , Homoscedasticity, R2, VIF, NORM ------> (φορεκατιστινγκ)

models_df <- allmodels(mod_data) # Find all models using the def all models
diagn_dataset <- Train_Dataset[,-((variabless+4):ncol(Train_Dataset))] # Data set for diagnostics without dummies CAUSE all Dummies = 0
diagnostics_df <- AutoHomo(models_df, diagn_dataset, y_name) # Find Diagnostics for all models using def
final_results <- cbind(models_df, diagnostics_df) # Combine Models with Diagnostics

# 4. Correlation matrix - Scatter plot

correlation_matrix <- cor(Full_Lagged_Data[, 2:(ncol(Full_Lagged_Data)-2)]) # Exclude DATE column
image(correlation_matrix, col = colorRampPalette(c("red", "green"))(100), axes = FALSE, main = "Correlation Matrix Heatmap")
axis(1, at = seq(0, 1, length.out = ncol(correlation_matrix)), labels = colnames(correlation_matrix), las = 2)
axis(2, at = seq(0, 1, length.out = nrow(correlation_matrix)), labels = colnames(correlation_matrix), las = 2) # Corr matrix

par(mfrow = c(2, 2)) # 2x2 grid

plot(Full_Lagged_Data$`Real GDP`, Full_Lagged_Data$`European GDP`, 
     main = "vs European GDP", xlab = "Real GDP", ylab = "Euro GDP", pch = 19, col = "blue") # European GDP scatterplot
plot(Full_Lagged_Data$`Real GDP`, Full_Lagged_Data$`Employment Rate`, 
     main = "vs Emp. Rate", xlab = "Real GDP", ylab = "Emp. Rate", pch = 19, col = "red") # Employment Rate scatterplot
plot(Full_Lagged_Data$`Real GDP`, Full_Lagged_Data$`Crude Oil/brent`, 
     main = "vs Crude Oil", xlab = "Real GDP", ylab = "Crude Oil", pch = 19, col = "green") # Crude Oil scatterplot
plot(Full_Lagged_Data$`Real GDP`, Full_Lagged_Data$`Receipt Travel`, 
     main = "vs Crude Oil", xlab = "Real GDP", ylab = "Receipt Travel", pch = 19, col = "purple") # Receipt Travels scatterplot
par(mfrow = c(1, 1)) # Reset to normal Grid 1x1 

# 5. Sort Models based on criteria

sorted_results <- final_results %>%

  mutate(Is_Strictly_Valid = Autocorrelation_Pval > border & Homoscedasticity_Pval > border & Normality_Pval > border & VIF_Value < 5) %>%
  arrange(desc(Is_Strictly_Valid), AIC_Value, desc(R_Squared)) %>%
  select(-any_of(dummy_cols)) %>%
  select(everything(), AIC_Value, Autocorrelation_Pval, R_Squared, Homoscedasticity_Pval, Is_Strictly_Valid)


# 6. Find the F statistic, MAE and MSPE of the valid models -------> ((Predicting))

valid_models <- subset(sorted_results, Is_Strictly_Valid)
valid_temp <- valid_models[, 1:variabless]
Model_Estimate <- ModelEstimate(valid_temp, Train_Dataset, y_name) # Estimate their F-statistic and R2
final_models_est <- cbind(valid_models,Model_Estimate) # Combine with valid models
MAE_MSPE <- MaeMspe(valid_temp, Train_Dataset, y_name, Predict_Dataset) # Find their MAE and MSPE
Models_ending <- cbind(final_models_est,MAE_MSPE) # Combine all together

best_models <- Models_ending %>%
  mutate(
    Is_Strictly_Valid = (
      Autocorrelation_Pval > border & 
        Homoscedasticity_Pval > border & 
        Normality_Pval > border & 
        VIF_Value < 5 &
        Model_Fpval < border
    )
  ) %>%
  arrange(desc(Is_Strictly_Valid), AIC_Value, MSPE_val) %>% 
  select(-any_of(dummy_cols)) %>%
  select(
    starts_with("Var"), 
    Is_Strictly_Valid, 
    AIC_Value, 
    MSPE_val, 
    Adjusted_R2, 
    Model_Fpval, 
    everything()
  )

# 7. Pick the first best model based on the LEAST MSPE and MAE and do final predictions and plot

model_1 <- lm(`Real GDP` ~ `Crude Oil/brent` + `German Searches` + `Employment Rate` + `Retail Trade` + `Consumer` + `10Y BOND INT`, 
              data = Train_Dataset) # Best Model
predictions_s <- predict(model_1, newdata = Predict_Dataset) # Final Predictions
comparison <- data.frame(Date = Predict_Dataset$Date, Actual_GDP = Predict_Dataset$`Real GDP`, Predicted_GDP = predictions_s) # Compare Actual vs Predicted

# residuals <- comparison$Actual_GDP - comparison$Predicted_GDP # Find the residuals from predicting to real
# MSPE <- mean(residuals^2) # Find MSPE of Model
# MAE <- mean(abs(residuals)) # Find MAE of Model

ggplot(comparison, aes(x = Date)) +
  geom_line(aes(y = Actual_GDP, color = "Actual"), linewidth = 1.2) +
  geom_line(aes(y = Predicted_GDP, color = "Predicted"), linewidth = 1.2, linetype = "dashed") +
  scale_color_manual(values = c("Actual" = "black", "Predicted" = "red")) +
  labs(title = "Final Forecast Validation", y = "Real GDP", x = "Date") +
  theme_minimal()