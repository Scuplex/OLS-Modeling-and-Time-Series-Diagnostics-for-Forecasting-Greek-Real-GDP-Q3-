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

# adjusted r squared
summary_model <- summary(model)
adjusted_r_squared <- summary_model$adj.r.squared


No dummies 
# Stationarity The Variables IF needed
#results_list <- make_stationary(data, border, lagvar) # Def for stationarity
#final_data <- results_list$final_data # Extract Final Data
#final_adf_values <- results_list$ADF_pvalues # Extract ADF P-values



# ask delis

use shapiro?

#normality test
test_norm_KS<-ks.test(resid(model_1), pnorm)

#n
X_ALL<-diff(as.matrix(MASTER_FILE_R[,3:ncol(MASTER_FILE_R)]))
X_ALL_LAG<- lag(X_ALL,1)
Y<-as.matrix(Y_gr_log[2:nrow(Y_gr_log),1])
names(Y)<- c('GDP_growth')
DATASET <- data.frame(Y,X_ALL_LAG)
DATASET <- data.frame(DATE[2:nrow(DATE),1],Y,X_ALL_LAG)

model_1 <- lm(GDP_growth - 1 + IP1 + IP2 + IP3,
              data = DATASET)

#SPAW TO DEIGMA
end_in_sample< VALE TO DATE POU THES NA STAMATISEI
DATASET_IN_SAMPLE<- DATASET[DATASET[,1]<end_in_sample,]

#SEED SPERM OILS#
DUMMY_COVID <- as.matrix(0,nrow = nrow(DATASET),ncol=1)
DUMMY_COVID[100:101,1] <- 1

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