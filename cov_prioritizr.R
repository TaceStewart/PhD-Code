# Load libraries
library(ggplot2)
library(terra)
library(prioritizr)
library(gridExtra)

# Load data
setwd("~/PhD/PhD-Code")
grid <- read.csv("cov_species_data.csv") #"dummy_dataset.csv"

# Get nrow and ncol from grid
nrow <- ceiling(max(grid$x))
ncol <- ceiling(max(grid$y))

# Example species data and cost data
# For simplicity, let's assume species_presence represents binary data
species_data <- rast(matrix(grid$species_present, 
                            nrow = nrow, 
                            ncol = ncol))
cost_data <- rast(matrix(rep(1, nrow * ncol), 
                         nrow = nrow, 
                         ncol = ncol)) # uniform cost for all cells

# Plot species presence
plot(species_data, main = "Species Presence in Grid")

# 1a. Create a conservation problem
p <- problem(cost_data, species_data) %>%
  add_min_set_objective() %>%   # Minimize the cost while covering species
  add_relative_targets(0.3) %>% # Target 30% of each species
  add_binary_decisions() %>%    # Binary decision: protect or not protect
  add_default_solver()          # Use default solver

# Solve the problem
solution <- solve(p, force = TRUE)

# Plot the solution
plot(solution, main = "Optimised Protected Areas")

# 1b. Create a conservation problem with a budget constraint
budget_limit <- 0.3*nrow*ncol  # Set budget limit to the total number of cells

# Create a conservation problem where the objective is to maximise species coverage
problem_present <- problem(cost_data, species_data) %>%
  add_max_utility_objective(budget = budget_limit) %>%  # Maximise species coverage within budget
  add_binary_decisions() %>%  # Binary decision: protect or not
  add_default_solver()        # Use default solver

# Solve the problem
solution_present <- solve(problem_present, force = TRUE)

# Plot the solution
plot(solution_present, main = "Optimised Protected Areas")

# 2. Fit a logistic regression model using covariates as the predictor
library(dplyr)

# Fit a logistic regression model using multiple covariates
sdm_model <- glm(data = grid, 
                 species_present ~ cov1 + cov2 + cov3, 
                 family = binomial)

# Summarize the model
summary(sdm_model)

# Predict species presence for present temperature data
grid$predicted_species_present <- predict(sdm_model, 
                                          newdata = grid, 
                                          type = "response")

# Simulate future covariate data with randomised increases for each cell
grid$future_cov1 <- grid$cov1 + runif(n = nrow(grid), min = 0.5, max = 1.5)  # Random increase between 0.5 and 1.5
grid$future_cov2 <- grid$cov2 + runif(n = nrow(grid), min = 1, max = 2)      # Random increase between 1 and 2
grid$future_cov3 <- grid$cov3 + runif(n = nrow(grid), min = 2, max = 3)      # Random increase between 2 and 3

# Plot all covariates
cov1_plot <- ggplot(grid, aes(x = x, y = y)) +
  geom_tile(aes(fill = cov1)) +
  scale_fill_viridis_c() +
  coord_fixed() +
  theme_minimal() +
  labs(title = "Current Temperature", fill = "Temperature") +
  theme(plot.title = element_text(hjust = 0.5))
cov2_plot <- ggplot(grid, aes(x = x, y = y)) +
  geom_tile(aes(fill = cov2)) +
  scale_fill_viridis_c() +
  coord_fixed() +
  theme_minimal() +
  labs(title = "Current Precipitation", fill = "Precipitation") +
  theme(plot.title = element_text(hjust = 0.5))
cov3_plot <- ggplot(grid, aes(x = x, y = y)) +
  geom_tile(aes(fill = cov3)) +
  scale_fill_viridis_c() +
  coord_fixed() +
  theme_minimal() +
  labs(title = "Current Elevation", fill = "Elevation") +
  theme(plot.title = element_text(hjust = 0.5))
future_cov1_plot <- ggplot(grid, aes(x = x, y = y)) +
  geom_tile(aes(fill = future_cov1)) +
  scale_fill_viridis_c() +
  coord_fixed() +
  theme_minimal() +
  labs(title = "Future Temperature", fill = "Temperature") +
  theme(plot.title = element_text(hjust = 0.5))
future_cov2_plot <- ggplot(grid, aes(x = x, y = y)) +
  geom_tile(aes(fill = future_cov2)) +
  scale_fill_viridis_c() +
  coord_fixed() +
  theme_minimal() +
  labs(title = "Future Precipitation", fill = "Precipitation") +
  theme(plot.title = element_text(hjust = 0.5))
future_cov3_plot <- ggplot(grid, aes(x = x, y = y)) +
  geom_tile(aes(fill = future_cov3)) +
  scale_fill_viridis_c() +
  coord_fixed() +
  theme_minimal() +
  labs(title = "Future Elevation", fill = "Elevation") +
  theme(plot.title = element_text(hjust = 0.5))

# Arrange the plots side by side
covariates_plot <- grid.arrange(cov1_plot, cov2_plot, cov3_plot, 
                                future_cov1_plot, future_cov2_plot, future_cov3_plot, 
                                ncol = 3)

# Save the plot
ggsave("covariates.png", covariates_plot, width = 10, height = 5, dpi = 300)

# Predict species presence for future temperature
grid$predicted_species_future <- predict(sdm_model, 
                                         newdata = grid %>% 
                                           mutate(cov1 = future_cov1,
                                                  cov2 = future_cov2,
                                                  cov3 = future_cov3), 
                                         type = "response")

# Convert the future species projection into a raster format
future_species_rast <- rast(matrix(grid$predicted_species_future, 
                                   nrow = nrow, ncol = ncol))
present_species_rast <- rast(matrix(grid$predicted_species_present, 
                                    nrow = nrow, ncol = ncol))

# Plot present species in ggplot
present_spp_plot <- ggplot(as.data.frame(present_species_rast, xy = TRUE), 
                           aes(x = x, y = y)) +
  geom_tile(aes(fill = values(present_species_rast))) +
  scale_fill_viridis_c(limits = c(0, max(values(present_species_rast), 
                                         values(future_species_rast)))) +
  coord_fixed() +
  theme_minimal() +
  labs(title = "Present Species Presence", fill = "Presence") +
  theme(plot.title = element_text(hjust = 0.5))

# Plot future species in ggplot
future_spp_plot <- ggplot(as.data.frame(future_species_rast, xy = TRUE), 
                          aes(x = x, y = y)) +
  geom_tile(aes(fill = values(future_species_rast))) +
  scale_fill_viridis_c(limits = c(0, max(values(present_species_rast), 
                                         values(future_species_rast)))) +
  coord_fixed() +
  theme_minimal() +
  labs(title = "Future Species Presence", fill = "Presence") +
  theme(plot.title = element_text(hjust = 0.5))

# Arrange the plots side by side
present_vs_future_species <- grid.arrange(present_spp_plot, 
                                          future_spp_plot, ncol = 2)

# Save the plots
ggsave("present_vs_future_species.png", 
       present_vs_future_species, 
       width = 10, height = 5, dpi = 300)

# Use future species presence data as an input feature in prioritizr
problem_future <- problem(cost_data, future_species_rast) %>%
  add_max_utility_objective(budget = budget_limit) %>%
  add_binary_decisions() %>%
  add_default_solver()

# Solve the problem
solution_future <- solve(problem_future, force = TRUE)

# Plot the optimised areas based on future species presence
plot(solution_future, 
     main = "Protected Areas Based on Projected Future Species Presence")

# 3. Compare the solutions
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

# Plot the present solution
present_plot <- plot(solution_present, 
                     main = "Protected Areas Based on Current Species Presence")

# Plot the future solution
future_plot <- plot(solution_future, 
                    main = "Protected Areas Based on Future Species Presence")

# Arrange plots together

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

library(gridExtra)  # To arrange plots side by side

# Plot for present solution
p1 <- ggplot(solution_present_df, aes(x = x, y = y)) +
  geom_tile(aes(fill = protection_present)) +
  scale_fill_viridis_c() +
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
  scale_fill_viridis_c() +
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
ggsave("present_vs_future_protection.png", 
       arrangeGrob(p1, p2, ncol = 2), 
       width = 10, height = 5, dpi = 300)

