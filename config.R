# CHANGE BELOW

file_path <- "Data.xlsx" # Read the data CHANGE PATHS  
border <- 0.10 # Initialize the border you want for the P-value CHANGE YOUR BORDER
lagvar <- 8 # Initialize how many Variables you have for percentage change and NOT diff # CHANGE THE VALUES YOU HAVE TO LAG
dummy_cols <- c("Dcovid","Drebound") # dummies
variabless <- 14 # amount of variables you are using
n_train_new <- 42 # Train Data set size

# CHANGE ABOVE