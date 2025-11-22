source("libraries.R")
source("config.R")
source("stationarity_utils.R")
source("allmodels.R")
source("AutoHomo.R")

# Initialize Variables
data <- read_excel(file_path) 
variables <- ncol(data) - 1 # Find the number of the variables i have based on the Excel Format
ending <- ncol(data) # Go till the last variable in the Excel Format
current_data <- data # hold the current_data for temporary in the loop
variables <- ncol(current_data) - 1 # Find the Amount of Variables we will use for the regression -1 due to the date
ending <- ncol(current_data) # Ending Variable
y_name <- names(data)[2] # Dependent Variable Name

# Step 1, Stationarity def
results_list <- make_stationary(data, border, lagvar) # Def for stationarity
final_data <- results_list$final_data # Extract Final Data
final_adf_values <- results_list$ADF_pvalues # Extract ADF P-values

# Find All models
models_df <- allmodels(final_data)

# Autocorrelation and Homoscedasticity tests for all models
diagnostics_df <- AutoHomo(models_df, final_data, y_name)
final_results <- cbind(models_df, diagnostics_df)

# Step 2, Correlation matrix
correlation_matrix <- cor(final_data[, 2:ending])

# Step 3 scatter-plot

plot_data <- final_data %>%
  select(-DATE) %>%                    # remove DATE column
  select(`Real GDP`, everything())     # ensure Real GDP is first

plot_data_long <- plot_data %>%
  pivot_longer(
    cols = -`Real GDP`,
    names_to = "Variable",
    values_to = "Value"
  )

ggplot(plot_data_long, aes(x = Value, y = `Real GDP`)) +
  geom_point(color = "steelblue") +
  facet_wrap(~ Variable, scales = "free_x") +
  theme_minimal(base_size = 10) +
  labs(
    title = "Scatterplots of Real GDP vs Explanatory Variables",
    x = "Explanatory Variables",
    y = "Real GDP"
  )

# adjusted r squared
summary_model <- summary(model)
adjusted_r_squared <- summary_model$adj.r.squared

# Print Menu

print(final_adf_values) # Print the p-values
print(vif_values) # Print the multicollinearity
print(correlation_matrix) # Print the correlation
