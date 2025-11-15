library(readxl)
library(tseries)
library(car)
library(lmtest)
library(ggplot2)
library(forecast)
library(dplyr)
library(car)
library(stats)
library(dplyr)

# read Data

data <- read_excel("/Users/george/Documents/Project/Data.xlsx")

# Variables
# y <- as.matrix(data$`Real GDP`)
# x1 <- as.matrix(data$`Receipt travels`)
# x2 <- as.matrix(data$`N of Travel`)
# x3 <- as.matrix(data$`CPI QQ`)
# x4 <- as.matrix(data$`Crude Oil/brent`)
# x5 <- as.matrix(data$`Employment Rate`)
# x6 <- as.matrix(data$`EXPORT GOOD/SER`)


# Step 1, stationarity # +2 , +1

ADF_pvalues <- numeric(11)
names(ADF_pvalues) <- colnames(data)[2:12]
diff_data <- data.frame(DATE = data$DATE[-1])
diff_data_2 <- data.frame(DATE = data$DATE[-(1:2)])


for (i in 2:12) # Find the P-Values for each Variable
{ 
  ADF_result <- adf.test(as.numeric(data[[i]]))
  ADF_pvalues[i - 1] <- ADF_result$p.value
}


for (i in 1:11) # If P-value > 0.05 then diff
{
  if (i <= 5){
    if (ADF_pvalues[i] < 0.05) {
      diff_data[[names(ADF_pvalues)[i]]] <- data[[i + 1]][-1]
    } 
    else {
      x <- data[[i + 1]]
      pct_change <- (x - dplyr::lag(x, 1)) / dplyr::lag(x, 1)
      diff_data[[names(ADF_pvalues)[i]]] <- pct_change[-1]
    }
  }
  else {
    if (ADF_pvalues[i] < 0.05) {
      diff_data[[names(ADF_pvalues)[i]]] <- data[[i + 1]][-1]
    }
    else{
      diff_data[[names(ADF_pvalues)[i]]] <- diff(data[[i + 1]]) 
    }
  }
}

for (i in 2:12)  # Find the P-Values for each Variable AGAIN
{
  series <- as.numeric(diff_data[[i]])
  ADF_result <- adf.test(series)
  ADF_pvalues[i - 1] <- ADF_result$p.value
}


for (i in 1:11) # If P-value > 0.05 then diff
{
  if (i <= 5){
    if (ADF_pvalues[i] < 0.05) {
      diff_data_2[[names(ADF_pvalues)[i]]] <- diff_data[[i + 1]][-1]
    } 
    else {
      x <- diff_data[[i + 1]]
      pct_change <- (x - dplyr::lag(x, 1)) / dplyr::lag(x, 1)
      diff_data_2[[names(ADF_pvalues)[i]]] <- pct_change[-1]
    }
  }
  else {
    if (ADF_pvalues[i] < 0.05) {
      diff_data_2[[names(ADF_pvalues)[i]]] <- diff_data[[i + 1]][-1]
    }
    else{
      diff_data_2[[names(ADF_pvalues)[i]]] <- diff(diff_data[[i + 1]]) 
    }
  }
}

for (i in 2:12) # Find the P-Values for each Variable
{
  series <- as.numeric(diff_data_2[[i]])
  ADF_result <- adf.test(series)
  ADF_pvalues[i - 1] <- ADF_result$p.value
}

# Step 2, Correlation matrix
correlation_matrix <- cor(diff_data_2[, 2:12])

# Step 2, Multicollinearity, polisigramikotita
model <- lm(diff_data_2$`Real GDP` ~ ., data = diff_data_2[, 3:12])  
vif_values <- vif(model)

# Step 3 scatter-plot

plot_data <- diff_data_2 %>%
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

# Print menu
print(ADF_pvalues) # Print the p-values
print(vif_values) # Print the multicollinearity
print(correlation_matrix) # Print the correlation
