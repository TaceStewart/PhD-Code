# Function to create a dummy dataset for species presence and temperature data
create_dummy_dataset <- function(grid_size = 5,
                                 species_prob = 0.5, 
                                 temp_sensitivity = 1,  # Controls the strength of the preference
                                 optimal_temp = 21,  # Preferred temperature range
                                 temp_range = 3,     # Range around the optimal temperature 
                                 num_hotspots = 2,
                                 hotspot_temp = 40) {
  
  grid <- expand.grid(x = 1:grid_size, y = 1:grid_size)
  
  # --- Create hotspots ---
  hotspot_indices <- sample(1:nrow(grid), num_hotspots)
  grid$temperature <- 20
  grid$temperature[hotspot_indices] <- hotspot_temp
  
  for (i in hotspot_indices) {
    distances <- sqrt((grid$x - grid$x[i])^2 + (grid$y - grid$y[i])^2)
    temp_decrease <- hotspot_temp * exp(-distances / grid_size)
    grid$temperature <- pmax(grid$temperature, temp_decrease)
  }
  
  # --- Calculate species presence probability based on temperature ---
  # Gaussian function to model temperature preference
  grid$species_prob <- species_prob * 
    exp(-((grid$temperature - optimal_temp)^2) / (2 * temp_range^2))
  grid$species_prob <- pmin(grid$species_prob, 1)  
  grid$species_prob <- pmax(grid$species_prob, 0)  
  
  # --- Assign species presence based on calculated probability ---
  grid$species_present <- rbinom(nrow(grid), 1, grid$species_prob)
  
  return(grid)
}

# Example usage
my_dataset <- create_dummy_dataset(grid_size = 25)

# Plot species presence
library(ggplot2)
spp_plot <- ggplot(my_dataset, aes(x = x, y = y)) +
  geom_tile(aes(fill = as.factor(species_present))) +
  scale_fill_manual(values = c("white", "darkgreen"), 
                    labels = c("Absent", "Present")) +
  theme_minimal() +
  labs(title = "Species Presence in Grid", 
       fill = "Species")

# Plot temperature
temp_plot <- ggplot(my_dataset, aes(x = x, 
                                    y = y)) +
  geom_tile(aes(fill = temperature)) +
  scale_fill_gradient(low = "lightblue", 
                      high = "red") +
  theme_minimal() +
  labs(title = "Temperature Across Grid", 
       fill = "Temperature")

# --- Create future temperature ---
min_temp_ch <- 3
max_temp_ch <- 10
grid <- my_dataset
grid_size <- ceiling(max(grid$y))
species_prob <- 0.4
optimal_temp <- 21
temp_range = 3
hotspot_index <- sample(1:nrow(grid), 1)
grid$future_temp <- grid$temperature
grid$future_temp[hotspot_index] <- grid$temperature[hotspot_index] +
  max_temp_ch # Create new hotspot with increased temperature
distances <- sqrt((grid$x - grid$x[hotspot_index])^2 + 
                    (grid$y - grid$y[hotspot_index])^2)
temp_decrease <- grid$future_temp[hotspot_index] * exp(-distances / grid_size)
grid$future_temp <- pmax(grid$temperature + 3, temp_decrease) +
  rnorm(nrow(grid), mean = 0, sd = 1) # Add some noise

# --- Populate future species presence ---
grid$future_species_prob <- species_prob * 
  exp(-((grid$future_temp - optimal_temp)^2) / (2 * temp_range^2))
grid$future_species_prob <- pmin(grid$future_species_prob, 1)
grid$future_species_prob <- pmax(grid$future_species_prob, 0)
grid$future_species_present <- rbinom(nrow(grid), 1, grid$future_species_prob)

# Create a violin plot
my_dataset$species_present_lbl <- factor(my_dataset$species_present, 
                                     labels = c("Absent", "Present"))

violin_plot <- ggplot(my_dataset,
                      aes(x = species_present_lbl, 
                          y = temperature, 
                          fill = species_present_lbl)) +
  geom_violin(trim = FALSE) +
  scale_fill_manual(values = c("Absent" = "white", 
                               "Present" = "darkgreen")) +
  theme_minimal() +
  labs(title = "Temperature Distribution for Species Presence",
       x = "Species Presence",
       y = "Temperature") +
  theme(legend.position = "none")

# Save plots
ggsave("species_presence.png", spp_plot, width = 6, height = 6)
ggsave("temperature.png", temp_plot, width = 6, height = 6)
ggsave("violin_plot.png", violin_plot, width = 6, height = 6)

# Save as CSV
write.csv(grid, "dummy_dataset.csv", row.names = FALSE)
