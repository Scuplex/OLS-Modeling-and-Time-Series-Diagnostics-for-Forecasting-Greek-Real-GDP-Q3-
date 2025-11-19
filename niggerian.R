library(readxl)
#install.packages('tseries')
library(tseries)
#install.packages('ggplot2')
library(ggplot2)
library(lmtest)
library(car)
#install.packages('forecast')
library(forecast)
#install.packages('dplyr')
library('dplyr')

GPD_Data <- read_excel("~/Downloads/GPD_Data.xlsx", 
                       sheet = "Master")

#inserting data into variables fro, y,x1 to x14

#y <- as.matrix(GPD_Data[,2])
#x1 <- as.matrix(GPD_Data[,3])
#x2<- as.matrix(GPD_Data[,4])
#x3 <- as.matrix(GPD_Data[,5])
#x4 <- as.matrix(GPD_Data[,6])
#x5 <- as.matrix(GPD_Data[,7])
#x6 <- as.matrix(GPD_Data[,8])
#x7 <- as.matrix(GPD_Data[,9])
#x8 <- as.matrix(GPD_Data[,10])
#x9 <- as.matrix(GPD_Data[,11])
#x10 <- as.matrix(GPD_Data[,12])
#x11 <- as.matrix(GPD_Data[,13])
#x12 <- as.matrix(GPD_Data[,14])


#step 1 stationarity

ADF_pvalues <- numeric(12) # vector with p_values
names(ADF_pvalues)<-colnames(GPD_Data)[2:13] #name the values by using the colnames command
diff_data <- data.frame(observation_date=GPD_Data$observation_date[-1])#create a dataframe for the differential data

for (i in 2:13)
{
  ADF_result <- adf.test(as.numeric(GPD_Data[[i]])) # adf test per variable
  ADF_pvalues[i-1] <-ADF_result$p.value #stores only the P values from ADF test
}

for ( i in 1:12) # 1st differentials
{
  if (i < 5) # the first five are percebtages so we do diff
  {
    if (ADF_pvalues[i] < 0.05) { # test
      diff_data[[names(ADF_pvalues)[i]]] <- GPD_Data[[i + 1]][-1] #if ok then input in dataset
    }else
      diff_data[[names(ADF_pvalues)[i]]] <- diff(GPD_Data[[i + 1]]) #if p value > a then diff and then input
  }else{
    if (ADF_pvalues[i] < 0.05) {
      diff_data[[names(ADF_pvalues)[i]]] <- GPD_Data[[i + 1]][-1]
    } 
    else {
      x <- GPD_Data[[i + 1]] #temporary variable
      pct_change <- (x - dplyr::lag(x, 1)) / dplyr::lag(x, 1) #pcnt change since in ELSE
      diff_data[[names(ADF_pvalues)[i]]] <- pct_change[-1] #finally input the stationary after pcnt change in data
    }
  }
}

for (i in 2:13)
{
  ADF_result <- adf.test(as.numeric(diff_data[[i]])) # adf test per variable
  ADF_pvalues[i-1] <-ADF_result$p.value #stores only the P values from ADF test
}



# Step 2, Correlation matrix
correlation_matrix <- cor(diff_data[, 2:6])