Corellation matrix

# use image to show correlation
image(correlation_matrix, axes = FALSE, main = "Correlation Matrix Heatmap")
axis(1, at = seq(0, 1, length.out = ncol(correlation_matrix)), labels = colnames(correlation_matrix), las = 2)
axis(2, at = seq(0, 1, length.out = nrow(correlation_matrix)), labels = colnames(correlation_matrix), las = 2)

all(ADF_pvalues < border)

rhs <- paste0(rhs_basic, 
                " + dplyr::lag(`", dep_var_name, "`, 1)", 
                " + dplyr::lag(`", dep_var_name, "`, 2)")


# Step 2, Multicollinearity without N of Travel
model <- lm(`Real GDP` ~ ., data = final_data %>% select(-DATE)) 
vif_values <- vif(model)

# residuals autocorrelation
bg_test <- bgtest(model, order = 4) # make it so you can do it for all models save it into a numeric and find the best model use a2 loop i,j :)
bg_test <- bg_test$p.value
print(bg_test) # if p-value < 0.05 then we have autocorrelation

# residuals homoscedasticity
bptest_result <- bptest(model) #Breusch-Pagan test
bptest_pvalue <- bptest_result$p.value
print(bptest_pvalue)