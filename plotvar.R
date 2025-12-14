# plotting for powerpoint
library(ggplot2)
library(ggcorrplot)
library(corrplot)

par(mfrow = c(3, 1), mar = c(2, 4, 2, 2))
# 2. Plot Receipt Travels
plot(Full_Lagged_Data$Date, Full_Lagged_Data$`German Searches`, 
     type = "l", col = "blue", lwd = 2,
     main = "CPI", ylab = "RATE")

# 3. Plot German Searches
plot(Full_Lagged_Data$Date, Full_Lagged_Data$`Employment Rate`, 
     type = "l", col = "red", lwd = 2,
     main = "Crude Oil/Brent", ylab = "Oil")

# 4. Plot European GDP
plot(Full_Lagged_Data$Date, Full_Lagged_Data$`CPI QQ`, 
     type = "l", col = "darkgreen", lwd = 2,
     main = "10 YEAR BONT INT", ylab = "Rate")

# 5. Reset layout to normal
par(mfrow = c(1, 1))

#Scatterplot
par(mfrow = c(1, 1))

# Chart 1
plot(Full_Lagged_Data$`Real GDP`, Full_Lagged_Data$`European GDP`, 
     main = "vs European GDP", xlab = "Real GDP", ylab = "Euro GDP", pch = 19, col = "blue")

# Chart 2
plot(Full_Lagged_Data$`Real GDP`, Full_Lagged_Data$`Employment Rate`, 
     main = "vs Emp. Rate", xlab = "Real GDP", ylab = "Emp. Rate", pch = 19, col = "red")

# Chart 3
plot(Full_Lagged_Data$`Real GDP`, Full_Lagged_Data$`Crude Oil/brent`, 
     main = "vs Crude Oil", xlab = "Real GDP", ylab = "Crude Oil", pch = 19, col = "green")

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

