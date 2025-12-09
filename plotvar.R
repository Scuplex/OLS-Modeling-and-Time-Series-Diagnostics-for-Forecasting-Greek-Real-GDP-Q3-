# plotting for powerpoint

par(mfrow = c(3, 1), mar = c(2, 4, 2, 2))
# 2. Plot Receipt Travels
plot(Full_Lagged_Data$Date, Full_Lagged_Data$`CPI QQ`, 
     type = "l", col = "blue", lwd = 2,
     main = "CPI", ylab = "RATE")

# 3. Plot German Searches
plot(Full_Lagged_Data$Date, Full_Lagged_Data$`Crude Oil/brent`, 
     type = "l", col = "red", lwd = 2,
     main = "Crude Oil/Brent", ylab = "Oil")

# 4. Plot European GDP
plot(Full_Lagged_Data$Date, Full_Lagged_Data$`10Y BOND INT`, 
     type = "l", col = "darkgreen", lwd = 2,
     main = "10 YEAR BONT INT", ylab = "Rate")

# 5. Reset layout to normal
par(mfrow = c(1, 1))