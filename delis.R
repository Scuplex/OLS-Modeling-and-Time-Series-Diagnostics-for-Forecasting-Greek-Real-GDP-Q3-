source("libraries.R")
source("config.R")
source("stationarity_utils.R")

# Initialize Variables
data <- read_excel(file_path) 
variables <- ncol(data) - 1 # Find the number of the variables i have based on the Excel Format
ending <- ncol(data) # Go till the last variable in the Excel Format
current_data <- data # hold the current_data for temporary in the loop
variables <- ncol(current_data) - 1 # Find the Amount of Variables we will use for the regression -1 due to the date
ending <- ncol(current_data) # Ending Variable

# Step 1, Stationarity def
results_list <- make_stationary(data, border, lagvar) # Def for stationarity
final_data <- results_list$final_data # Extract Final Data
final_adf_values <- results_list$final_adf_values # Extract ADF P-values

# Step 2, Correlation matrix
correlation_matrix <- cor(final_data[, 2:ending])

# Step 2, Multicollinearity without N of Travel
model <- lm(`Real GDP` ~ ., data = final_data %>% select(-DATE)) 
vif_values <- vif(model)

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



# Your predictors (replace with your names if different)
predictors <- colnames(data)[3:ncol(data)]

# Make all combinations from size 1 to 10
combos <- unlist(
  lapply(1:length(predictors), function(k) combn(predictors, k, simplify = FALSE)),
  recursive = FALSE
)

# Turn each combination into a formula string
model_formulas <- sapply(combos, function(vars) {
  paste("Y", paste(vars, collapse = " + "))
})

# Put all models in a simple data frame
models_df <- data.frame(model = model_formulas)

# See the first few
head(models_df)






# adjusted r squared
summary_model <- summary(model)
adjusted_r_squared <- summary_model$adj.r.squared

# residuals autocorrelation
bg_test <- bgtest(model, order = 4) # make it so you can do it for all models save it into a numeric and find the best model use a2 loop i,j :)
bg_test <- bg_test$p.value
print(bg_test) # if p-value < 0.05 then we have autocorrelation

# residuals homoscedasticity
bptest_result <- bptest(model) #Breusch-Pagan test
bptest_pvalue <- bptest_result$p.value
print(bptest_pvalue)

# Normality test
shapiro_test <- shapiro.test(residuals(model)) # Shapiro-Wilk test

# f-test 
f_test <- summary_model$fstatistic # F-statistic

# Print Menu

print(final_adf_values) # Print the p-values
print(vif_values) # Print the multicollinearity
print(correlation_matrix) # Print the correlation
