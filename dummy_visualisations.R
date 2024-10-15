# Load library
library(ggplot2)

# Load data
setwd("~/PhD/PhDCode")
grid <- read.csv("dummy_dataset.csv")

# Plot species presence
ggplot(grid, aes(x = x, y = y)) +
  geom_tile(aes(fill = as.factor(species_present))) +
  scale_fill_manual(values = c("white", "darkgreen"), 
                    labels = c("Absent", "Present")) +
  theme_minimal() +
  labs(title = "Species Presence in Grid", 
       fill = "Species")

# Plot temperature
ggplot(grid, aes(x = x, 
                 y = y)) +
  geom_tile(aes(fill = temperature)) +
  scale_fill_gradient(low = "lightblue", 
                      high = "red") +
  theme_minimal() +
  labs(title = "Temperature Across Grid", 
       fill = "Temperature")

# Create a violin plot
grid$species_present <- factor(grid$species_present, 
                               labels = c("Absent", "Present"))
ggplot(grid, 
       aes(x = species_present, 
           y = temperature, 
           fill = species_present)) +
  geom_violin(trim = FALSE) +
  scale_fill_manual(values = c("Absent" = "white", 
                               "Present" = "darkgreen")) +
  theme_minimal() +
  labs(title = "Temperature Distribution for Species Presence",
       x = "Species Presence",
       y = "Temperature") +
  theme(legend.position = "none")