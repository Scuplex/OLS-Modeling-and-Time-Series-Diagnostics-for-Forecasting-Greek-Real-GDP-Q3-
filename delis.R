library(readxl)
library(tseries)
library(car)
library(lmtest)
library(ggplot2)
library(forecast)
library(car)
library(stats)
library(dplyr)
library(tidyr)
library(lmtest)

# Initialize VARIABLES 

# CHANGE BELOW

data <- read_excel("Data.xlsx") # Read the data CHANGE PATHS
border <- 0.05 # Initialize the border you want for the P-value CHANGE YOUR BORDER
lagvar <- 7 # Initialize how many Variables you have for percentage change and NOT diff # CHANGE THE VALUES YOU HAVE TO LAG
# CHANGE ABOVE


variables <- ncol(data) - 1 # Find the number of the variables i have based on the Excel Format
ending <- ncol(data) # Go till the last variable in the Excel Format
current_data <- data # hold the current_data for temporary in the loop
iteration <- 0 # for the while loop of the stationarity
variables <- ncol(current_data) - 1 # Find the Amount of Variables we will use for the regression -1 due to the date
ending <- ncol(current_data) # Ending Variable
ADF_pvalues <- numeric(variables) # Create a numeric to place in the ADF values based on the variables we have
names(ADF_pvalues) <- colnames(current_data)[2:ending] # Give the names to the ADF values 

y <- as.matrix(data$`Real GDP`)
# make the variable Y percentage changes using lag 

# Step 1, stationarity 

repeat { # Used repeat cause i wanna run the stationarity untill all stationed.It is the same as the do-while waits until break
  
  for (i in 2:ending) {
    ADF_result <- adf.test(as.numeric(current_data[[i]]))
    ADF_pvalues[i - 1] <- ADF_result$p.value # Find the P-values
  }
  
  if (all(ADF_pvalues < border)) {
    final_data <- current_data # Save the Last Dataset
    break # Stop the loop if all stationary
  }

  diff_data <- data.frame(DATE = current_data$DATE[-1]) # Build next dataset
  
  for (i in 1:variables) {
    name <- names(ADF_pvalues)[i]
    x <- current_data[[i + 1]]
    
    if (i <= lagvar && iteration == 0) { # && iteration == 0 BECAUSE after the first one the percentages MUST be diff not per changed
      if (ADF_pvalues[i] < border) {
        diff_data[[name]] <- x[-1] # KEEP ORIGINAL VALUES BUT REMOVE FIRST ROW
      } else {
        pct_change <- (x - dplyr::lag(x, 1)) / dplyr::lag(x, 1)# PERCENT CHANGE
        diff_data[[name]] <- pct_change[-1] # first value is NA that is why i apply -1
      }
    } else 
      {
      if (ADF_pvalues[i] < border) {
        diff_data[[name]] <- x[-1]# KEEP ORIGINAL VALUES BUT REMOVE FIRST value
      } else {
        diff_data[[name]] <- diff(x) # DIFFERENCE
      }
    }
  }
  current_data <- diff_data # Change the current_data to the diff_data for the next loop
  iteration <- iteration + 1   # Move to next iteration
}


# Step 2, Correlation matrix
correlation_matrix <- cor(final_data[, 2:ending])
# use image to show correlation
image(correlation_matrix, axes = FALSE, main = "Correlation Matrix Heatmap")
axis(1, at = seq(0, 1, length.out = ncol(correlation_matrix)), labels = colnames(correlation_matrix), las = 2)
axis(2, at = seq(0, 1, length.out = nrow(correlation_matrix)), labels = colnames(correlation_matrix), las = 2)

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

# adjusted r squared
summary_model <- summary(model)
adjusted_r_squared <- summary_model$adj.r.squared

# residuals autocorrelation
bg_test <- bgtest(model, order = 4) # make it so you can do it for all models save it into a numeric and find the best model use a2 loop i,j :)
bg_test <- bg_test$p.value
print(bg_test)

# residuals homoscedasticity
bptest_result <- bptest(model) #Breusch-Pagan test
bptest_pvalue <- bptest_result$p.value
print(bptest_pvalue)

# Normality test
shapiro_test <- shapiro.test(residuals(model)) # Shapiro-Wilk test

# f-test 
f_test <- summary_model$fstatistic # F-statistic

# Print Menu

print(ADF_pvalues) # Print the p-values
print(vif_values) # Print the multicollinearity
print(correlation_matrix) # Print the correlation
