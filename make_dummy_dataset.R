# Set grid size
grid_size <- 5  # Change to 10 for larger grids

# Create a data frame for grid cells
grid <- expand.grid(x = 1:grid_size, 
                    y = 1:grid_size)

# Assign species presence (1 for present, 0 for absent)
set.seed(123)  # For reproducibility
grid$species_present <- sample(c(0, 1), 
                               size = nrow(grid), 
                               replace = TRUE)

# Assign environmental variable (e.g., temperature)
grid$temperature <- round(runif(nrow(grid), 
                                min = 0, 
                                max = 100), 1)

# View the dataset
print(grid)

# Optionally, save to a CSV file
write.csv(grid, 
          "dummy_dataset.csv", 
          row.names = FALSE)
