# plotting for powerpoint
library(ggplot2)
library(ggcorrplot)
library(corrplot)

par(mfrow = c(3, 1), mar = c(2, 4, 2, 2))
# 2. Plot Receipt Travels
plot(Full_Lagged_Data$Date, Full_Lagged_Data$`Stock ATH`, 
     type = "l", col = "blue", lwd = 2,
     main = "Stock ATH", ylab = "RATE")

# 3. Plot German Searches
plot(Full_Lagged_Data$Date, Full_Lagged_Data$`IMPORT GOOD/ser`, 
     type = "l", col = "red", lwd = 2,
     main = "Employment Rate", ylab = "Oil")

# 4. Plot European GDP
plot(Full_Lagged_Data$Date, Full_Lagged_Data$`EXPORT GOOD/SER`, 
     type = "l", col = "darkgreen", lwd = 2,
     main = "EXPORT GOOD/SER", ylab = "Rate")

# 5. Reset layout to normal
par(mfrow = c(1, 1))

#Scatterplot
par(mfrow = c(1, 1))

# Chart 1
plot(Full_Lagged_Data$`Real GDP`, Full_Lagged_Data$`Stock ATH`, 
     main = "vs Stock ATH", xlab = "Real GDP", ylab = "Stock ATH", pch = 19, col = "blue")

# Chart 2
plot(Full_Lagged_Data$`Real GDP`, Full_Lagged_Data$`German Searches`, 
     main = "vs German Searches", xlab = "Real GDP", ylab = "German Searches", pch = 19, col = "red")

# Chart 3
plot(Full_Lagged_Data$`Real GDP`, Full_Lagged_Data$`Y_LAGGED`, 
     main = "vs Y_LAGGED", xlab = "Real GDP", ylab = "Y_LAGGED", pch = 19, col = "green")

# Reset layout to normal
par(mfrow = c(1, 1))

correlation_matrix <- cor(Full_Lagged_Data[, 2:(ncol(Full_Lagged_Data)-2)]) 

# correlation plot ggcorr
ggcorrplot(correlation_matrix, 
           method = "square", 
           type = "lower",        # Shows the lower half
           lab = TRUE,            # Show the numbers
           lab_size = 3, 
           colors = c("darkred", "white", "darkblue"), 
           title = "Correlation Matrix", 
           ggtheme = ggplot2::theme_minimal())

