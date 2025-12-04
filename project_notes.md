# use image to show correlation
image(correlation_matrix, axes = FALSE, main = "Correlation Matrix Heatmap")
axis(1, at = seq(0, 1, length.out = ncol(correlation_matrix)), labels = colnames(correlation_matrix), las = 2)
axis(2, at = seq(0, 1, length.out = nrow(correlation_matrix)), labels = colnames(correlation_matrix), las = 2)

all(ADF_pvalues < border)

rhs <- paste0(rhs_basic, 
                " + dplyr::lag(`", dep_var_name, "`, 1)", 
                " + dplyr::lag(`", dep_var_name, "`, 2)")

# residuals autocorrelation
bg_test <- bgtest(model, order = 4) # make it so you can do it for all models save it into a numeric and find the best model use a2 loop i,j :)
bg_test <- bg_test$p.value
print(bg_test) # if p-value < 0.05 then we have autocorrelation

# residuals homoscedasticity
bptest_result <- bptest(model) #Breusch-Pagan test
bptest_pvalue <- bptest_result$p.value
print(bptest_pvalue)

# adjusted r squared
summary_model <- summary(model)
adjusted_r_squared <- summary_model$adj.r.squared


No dummies 
# Stationarity The Variables IF needed
#results_list <- make_stationary(data, border, lagvar) # Def for stationarity
#final_data <- results_list$final_data # Extract Final Data
#final_adf_values <- results_list$ADF_pvalues # Extract ADF P-values



# ADD

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
  
  
  # ask
  
  can i have million and perc change? i used diff log
  
  
  
  
  
  
  
  # notes to add
  
  Because i have a small sample i will use both ADF and KPSS tests to check for stationarity.
What is stationarity? (The goal of ADF & KPSS)

A time series is stationary if:

its mean doesn’t drift over time

its variance is stable

shocks fade instead of accumulating

2. ADF Test – Does the series HAVE a unit root?

ADF = Augmented Dickey–Fuller Test

Hypotheses

H₀: series has a unit root (non-stationary) p<0.05 reject

H₁: series is stationary



3. KPSS Test – Is the series STATIONARY around a mean or trend? 

KPSS = Kwiatkowski–Phillips–Schmidt–Shin

KPSS flips the hypotheses.

Hypotheses

H₀: series is stationary # p<0.05 reject

H₁: series is non-stationary


ADF sometimes fails to reject non-stationarity when the sample is small.

KPSS sometimes falsely detects non-stationarity when variance is high.


# employment_lagged add the name 


## na valw 1 dummy kai na valw 2-3 times oxi parapanw
# thelw ola ta f statistics to pvalue na einai mikro
# thelw kai ta pvalue ton metavlitwn sto modelo n einai statistika simantika pvalue < 0.05
# an exw b arnitiko dn einai kalo to model kai prepei na exw thetika B 
# adjusted R squared na einai megalo.

pws kanw provlepeis predict(mode,date>=)
kanw metavlites gia to out of sample and in sample 
