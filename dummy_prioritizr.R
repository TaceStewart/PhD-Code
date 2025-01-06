# Clear environment, console, and plots (if exists)
rm(list = ls())
if(!is.null(dev.list())) dev.off()
cat("\014")

# Load libraries
library(ggplot2)
library(terra)
library(prioritizr)
library(gridExtra)
library(dplyr)
library(viridis)

# Load data
setwd("~/PhD/PhD-Code")
grid <- read.csv("dummy_dataset.csv")

# Get nrow and ncol from grid
nrow <- ceiling(max(grid$x))
ncol <- ceiling(max(grid$y))
grid_size <- ncol

# --- Function to create raster from grid data ---
create_raster <- function(data) {
  rast(matrix(data, nrow = nrow, ncol = ncol))
}

# Example species data and cost data
species_data <- create_raster(grid$species_present)
cost_data <- create_raster(rep(1, nrow * ncol)) 

# --- Function for plotting rasters ---
plot_raster <- function(raster_data, title, fill_label,
                        min_value = min(values(raster_data)),
                        max_value = max(values(raster_data))) {
  ggplot(as.data.frame(raster_data, xy = TRUE), aes(x = x, y = y)) +
    geom_tile(aes(fill = values(raster_data))) +
    scale_fill_viridis(limits = c(min_value,
                                  max_value)) +
    coord_fixed() +
    theme_minimal() +
    labs(title = title, fill = fill_label) +
    theme(plot.title = element_text(hjust = 0.5))
}

# Plot species presence
obs_spp_plot <- plot_raster(species_data, 
                            "Observed Species Presence", 
                            "Presence")
obs_spp_plot

# Save the plot
ggsave("Outputs/Vis/dummy_observed_spp.png", obs_spp_plot, 
       width = 5, height = 5, dpi = 300)

# Plot cost data
cost_plot <- plot_raster(cost_data, "Cost Data", "Cost")
cost_plot

# Save the plot
ggsave("Outputs/Vis/dummy_cost_data.png", cost_plot, 
       width = 5, height = 5, dpi = 300)

## I think how the model is built or used is wrong - 
## It may be using the same data to create the exact same predicted species present

# Fit a logistic regression model
sdm_model <- glm(data = grid[, c("species_present", "temperature")], 
                 species_present ~ temperature, 
                 family = binomial)
summary(sdm_model)

# Predict species presence
grid$predicted_species_present <- predict(sdm_model, 
                                          newdata = grid[,  c("species_present", "temperature")], 
                                          type = "response")

# --- Create future temperature and predict future species presence ---
min_temp_ch <- 3
max_temp_ch <- 10
hotspot_index <- sample(1:nrow(grid), 1)
grid$pred_future_temp <- grid$temperature
grid$pred_future_temp[hotspot_index] <- grid$temperature[hotspot_index] +
  max_temp_ch # Create new hotspot with increased temperature
distances <- sqrt((grid$x - grid$x[hotspot_index])^2 + 
                    (grid$y - grid$y[hotspot_index])^2)
temp_decrease <- grid$pred_future_temp[hotspot_index] * exp(-distances / grid_size)
grid$pred_future_temp <- pmax(grid$temperature + 3, temp_decrease) +
  rnorm(nrow(grid), mean = 0, sd = 1) # Add some noise

# Plot current and future temperature
current_temp_data <- create_raster(grid$temperature)
pred_future_temp_data <- create_raster(grid$pred_future_temp)
current_temp_plot <- plot_raster(current_temp_data, 
                                 "Current Temperature", 
                                 "Temperature",
                                 min_value = min(grid$temperature,
                                                 grid$pred_future_temp),
                                 max_value = max(grid$temperature,
                                                 grid$pred_future_temp))
pred_future_temp_plot <- plot_raster(pred_future_temp_data,
                                "Predicted Future Temperature", 
                                "Temperature",
                                min_value = min(grid$temperature,
                                                grid$pred_future_temp),
                                max_value = max(grid$temperature,
                                                grid$pred_future_temp))

# Arrange and save the plots (current vs. future temperature)
temp_plot <- grid.arrange(current_temp_plot, pred_future_temp_plot, ncol = 2)
ggsave("Outputs/Vis/dummy_current_vs_pred_future_temp.png", temp_plot, 
       width = 10, height = 5, dpi = 300)

grid$predicted_species_future <- predict(sdm_model, 
                                         newdata = grid[, c("species_present", 
                                                            "pred_future_temp")] %>%
                                           mutate(temperature = pred_future_temp),
                                         type = "response")

# BEFORE MOVING FORWARD: Check if the model is predicting the same species presence
# as the future prediction (we don't want this)
identical(grid$predicted_species_present, grid$predicted_species_future)
# I think I did it!!

# Create rasters for predicted species presence
predicted_species_data <- create_raster(grid$predicted_species_present)
future_species_data <- create_raster(grid$predicted_species_future)

# Plot predicted species presence
pred_spp_plot <- plot_raster(predicted_species_data, 
                             "Predicted Species Presence", 
                             "Pr(Presence)")

# Arrange and save the plots (observed vs. predicted)
obs_vs_pred_spp <- grid.arrange(obs_spp_plot, 
                                pred_spp_plot, 
                                ncol = 2)
ggsave("Outputs/Vis/dummy_obs_vs_pred_spp.png", obs_vs_pred_spp, 
       width = 10, height = 5, dpi = 300)

# Plot present and future species presence
pres_vs_fut_spp <- grid.arrange(plot_raster(current_temp_data, 
                                            "Temperature (Current)", 
                                            "Temperature",
                                            min_value = min(grid$temperature,
                                                            grid$pred_future_temp),
                                            max_value = max(grid$temperature,
                                                            grid$pred_future_temp)), 
                                plot_raster(pred_future_temp_data, 
                                            "Temperature (Future)", 
                                            "Temperature",
                                            min_value = min(grid$temperature,
                                                            grid$pred_future_temp),
                                            max_value = max(grid$temperature,
                                                            grid$pred_future_temp)), 
                                plot_raster(predicted_species_data, 
                                            "Species Presence (Current)", 
                                            "Probability",
                                            min_value = 0,
                                            max_value = 1), 
                                plot_raster(future_species_data, 
                                            "Species Presence (Future)", 
                                            "Probability",
                                            min_value = 0,
                                            max_value = 1), 
                                ncol = 2)

# Save the plots
ggsave("Outputs/Vis/dummy_present_vs_future_spp.png", pres_vs_fut_spp, 
       width = 10, height = 5, dpi = 300)

# --- Conservation planning ---
budget_limit <- 0.2 * sum(values(cost_data)) 

# --- Function to create and solve conservation problem ---
make_conservation_problem <- function(species_raster) {
  problem(cost_data, species_raster) %>%
    add_max_utility_objective(budget = budget_limit) %>%
    add_binary_decisions() %>%
    add_default_solver()
}

# Set up conservation problems
problem_present <- make_conservation_problem(predicted_species_data)
problem_future <- make_conservation_problem(future_species_data)

# Solve for present and future
solution_present <- problem_present %>% solve(force = TRUE)
solution_future <- problem_future %>% solve(force = TRUE)

# Plot the solutions
sol_pres_plot <- plot_raster(solution_present, "Protected Areas (Present)", "Protection")
sol_fut_plot <- plot_raster(solution_future, "Protected Areas (Future)", "Protection")

# Save present plot
ggsave("Outputs/Vis/dummy_present_solution.png", sol_pres_plot, 
       width = 5, height = 5, dpi = 300)

# Arrange and save the plots (present vs. future protection)
pres_vs_fut_prot <- grid.arrange(sol_pres_plot, sol_fut_plot, ncol = 2)
ggsave("Outputs/Vis/dummy_present_vs_future_protection.png", 
       pres_vs_fut_prot, width = 10, height = 5, dpi = 300)

# ---  Solution comparison and analysis ---
# Calculate the total cost of each solution
cost_summary_present <- eval_cost_summary(problem_present, solution_present)
cost_summary_future <- eval_cost_summary(problem_future, solution_future)

# Calculate the total number of selected planning units
n_summary_present <- eval_n_summary(problem_present, solution_present)
n_summary_future <- eval_n_summary(problem_future, solution_future)

# Calculate the present species protection benefit for the present solution
objective_present_pv <- eval_feature_representation_summary(problem_present, 
                                                            solution_present)

# Calculate the present species protection benefit for the future solution
objective_future_pv <- eval_feature_representation_summary(problem_present, 
                                                           solution_future)


# Calculate the future species protection benefit for the present solution
objective_present_fv <- eval_feature_representation_summary(problem_future, 
                                                            solution_present)

# Calculate the future species protection benefit for the future solution
objective_future_fv <- eval_feature_representation_summary(problem_future, 
                                                           solution_future)

# Create a summary table
summary_table <- data.frame(Solution = c("Current Species Model", "Future Species Model"),
                            Total_Cost = c(cost_summary_present$cost, 
                                           cost_summary_future$cost),
                            Total_Selected_PAs = c(n_summary_present$n, 
                                                   n_summary_future$n),
                            Present_Benefit = c(objective_present_pv$absolute_held, 
                                                objective_future_pv$absolute_held),
                            Future_Benefit = c(objective_present_fv$absolute_held, 
                                               objective_future_fv$absolute_held))

# Calculate overlap between the two solutions
overlap <- solution_present * solution_future

# Calculate percentage overlap
overlap_percentage <- sum(values(overlap)) / sum(values(solution_present)) * 100
cat("Percentage overlap between current and future solutions:", 
    overlap_percentage, "%\n")

# Plot the overlap
plot(overlap, 
     main = "Overlap Between Current and Future Solutions")


# Show both plots side by side with present and future benefit values
# Convert rasters to data frames
solution_present_df <- as.data.frame(solution_present, xy = TRUE)
solution_future_df <- as.data.frame(solution_future, xy = TRUE)

# Convert species presence rasters for plotting (current and future)
present_species_rast <- rast(matrix(grid$species_present, 
                                    nrow = nrow, ncol = ncol))
future_species_rast <- rast(matrix(grid$predicted_species_future, 
                                   nrow = nrow, ncol = ncol))
species_present_df <- as.data.frame(present_species_rast, xy = TRUE)
species_future_df <- as.data.frame(future_species_rast, xy = TRUE)

# Rename columns for clarity
colnames(solution_present_df) <- c("x", "y", "protection_present")
colnames(solution_future_df) <- c("x", "y", "protection_future")
colnames(species_present_df) <- c("x", "y", "species_present")
colnames(species_future_df) <- c("x", "y", "species_future")

# Plot for present solution
p1 <- ggplot(solution_present_df, aes(x = x, y = y)) +
  geom_tile(aes(fill = protection_present)) +
  scale_fill_viridis() +
  coord_fixed() +
  theme_minimal() +
  labs(title = "Present Protection", 
       subtitle = paste("Future Benefit:",
                        round(objective_present_fv$absolute_held, 2)),
       fill = "Protection") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Plot for future solution
p2 <- ggplot(solution_future_df, aes(x = x, y = y)) +
  geom_tile(aes(fill = protection_future)) +
  scale_fill_viridis() +
  coord_fixed() +
  theme_minimal() +
  labs(title = "Future Protection", 
       subtitle = paste("Future Benefit:",
                        round(objective_future_fv$absolute_held, 2)),
       fill = "Protection") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Arrange the two plots side by side
grid.arrange(p1, p2, ncol = 2)

# Save the plots
ggsave("Outputs/Vis/dummy_present_vs_future_protection.png", 
       arrangeGrob(p1, p2, ncol = 2), 
       width = 10, height = 5, dpi = 300)

# Make a side-by-side plot of species presence heat map and species presence/temperature bar plot
species_heatmap <- ggplot(grid, aes(x = x, y = y)) +
  geom_tile(aes(fill = as.factor(species_present))) +
  scale_fill_manual(values = c("lightgrey", "darkgreen"), 
                    labels = c("No Observed Presence", "Observed Presence")) +
  theme_minimal() +
  labs(fill = "Species Presence") +
  theme(legend.text = element_text(size = 8),
        legend.position = "bottom",
        axis.title = element_blank(),
        axis.text = element_blank())

ts_plot <- ggplot(grid, aes(x = temperature, 
                            group = as.factor(species_present),
                            fill = as.factor(species_present))) +
  geom_histogram(bins = 20) +
  scale_fill_manual(values = c("lightgrey", "darkgreen"), 
                    labels = c("No Observed Presence", "Observed Presence")) +
  theme_minimal() +
  labs(x = "Temperature",
       y = "Frequency",
       fill = "Species Presence") +
  theme(legend.text = element_text(size = 8),
        legend.position = "bottom")

# Arrange and save the plots
grid.arrange(species_heatmap, ts_plot, ncol = 2)
ggsave("Outputs/Vis/dummy_species_presence_vs_temperature.png", 
       arrangeGrob(species_heatmap, ts_plot, ncol = 2), 
       width = 10, height = 5, dpi = 300)
