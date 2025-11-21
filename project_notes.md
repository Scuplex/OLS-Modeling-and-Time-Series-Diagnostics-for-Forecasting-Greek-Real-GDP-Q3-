Corellation matrix

# use image to show correlation
image(correlation_matrix, axes = FALSE, main = "Correlation Matrix Heatmap")
axis(1, at = seq(0, 1, length.out = ncol(correlation_matrix)), labels = colnames(correlation_matrix), las = 2)
axis(2, at = seq(0, 1, length.out = nrow(correlation_matrix)), labels = colnames(correlation_matrix), las = 2)

