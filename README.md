# Greece GDP Forecasting Model

## 📌 Project Overview
This project aims to predict the **Gross Domestic Product (GDP) of Greece** using an Ordinary Least Squares (OLS) regression framework. By analyzing key macroeconomic indicators—such as energy costs, tourism demand, and market sentiment—we establish a statistically valid model to forecast economic growth.

The core objective is to identify a parsimonious model that balances **explanatory power ($R^2$)** with **predictive efficiency (AIC)**, ensuring all econometric assumptions (homoscedasticity, no autocorrelation, normality) are strictly met.

## 📊 Key Findings
After testing multiple model specifications, **Model 1** was selected as the optimal forecasting tool:
* **Predictors:** Crude Oil (Brent), German Search Volume (Tourism Proxy), Employment Rate, Retail Trade, Consumer Confidence, 10Y Bond Yields.
* **Performance:** Explained ~43% of GDP variation ($R^2 = 0.43$) with high statistical validity.
* **Reliability:** The model passed all diagnostic tests, including the Breusch-Godfrey test for autocorrelation and the Shapiro-Wilk test for normality.

## 📂 Repository Structure

The project is designed with a modular structure for easy debugging and scalability:

### **1. Core Execution**
* **`allmodels.R`**: The main execution script. Run this to load data, estimate all candidate models, and generate the comparison table.
* **`config.R`**: Global configuration settings (paths, constants).
* **`libraries.R`**: Loads all necessary R packages and dependencies.

### **2. Modeling & Analysis**
* **`models_estimate.R`**: Defines and estimates the different OLS model specifications (Model 1, Model 2, Model 3, etc.).
* **`MAE_MSPE.R`**: Calculates forecasting error metrics (Mean Absolute Error, Mean Squared Prediction Error) to evaluate accuracy.

### **3. Diagnostics & Testing**
* **`stationarity_utils.R`**: Performs Augmented Dickey-Fuller (ADF) and Phillips-Perron (PP) tests to check for unit roots and determine if differencing is needed.
* **`AutoHomo.R`**: Runs critical diagnostic checks:
    * *Autocorrelation:* Breusch-Godfrey Test
    * *Homoscedasticity:* Breusch-Pagan Test
    * *Normality:* Shapiro-Wilk Test

### **4. Visualization**
* **`plotvar.R`**: Generates plots to visualize variable trends and "Actual vs. Predicted" GDP graphs.

### **5. Data**
* **`Data.xlsx - Φύλλο1.csv`**: The raw dataset containing quarterly/yearly macroeconomic indicators for Greece.

---

## 🚀 Getting Started

### **Prerequisites**
Ensure you have **R** installed. You will need the following libraries (managed in `libraries.R`):
* `readxl`, `readr` (Data import)
* `lmtest`, `sandwich`, `tseries` (Econometrics & Testing)
* `ggplot2` (Visualization)
* `car` (VIF & Diagnostics)

### **Installation & Usage**
1.  **Clone the repository:**
    ```bash
    git clone [https://github.com/yourusername/greece-gdp-forecast.git](https://github.com/yourusername/greece-gdp-forecast.git)
    ```
2.  **Set your working directory** to the project folder.
3.  **Run the main script:**
    Open `allmodels.R` in RStudio and execute the code. It will:
    * Load the clean data.
    * Check variables for stationarity.
    * Run the regressions.
    * Output the "Best Model" summary table.

---

## 📈 Methodology
Our approach follows a strict econometric pipeline:
1.  **Stationarity Checks:** All variables are tested for unit roots. Non-stationary variables are differenced to avoid spurious regression results.
2.  **OLS Estimation:** We estimate multiple linear models using different combinations of predictors.
3.  **Diagnostic Validation:** We discard any model that violates Gauss-Markov assumptions (e.g., if residuals are not normal or show serial correlation).
4.  **Model Selection:** The "Winner" is chosen based on the lowest **AIC** (Akaike Information Criterion) and the highest **Adjusted $R^2$**.

## 📝 License
This project is open for educational and research purposes.

---