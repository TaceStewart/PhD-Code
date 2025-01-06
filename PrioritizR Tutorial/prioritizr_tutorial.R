# Spatial prioritisation tutorial at https://prioritizr.net/
#install.packages("prioritizr", repos = "https://cran.rstudio.com/")
# load packages
library(prioritizr)
library(prioritizrdata)
library(terra)

# import planning unit data
wa_pu <- get_wa_pu()

# preview data
print(wa_pu)

# plot data
plot(wa_pu, main = "Costs", axes = FALSE)

# import feature data
wa_features <- get_wa_features()

# preview data
print(wa_features)

# plot the first nine features
plot(wa_features[[1:9]], nr = 3, axes = FALSE)

# if needed, install HiGHS solver
# install.packages("highs", repos = "https://cran.rstudio.com/")
library(highs)

# calculate budget
budget <- terra::global(wa_pu, "sum", na.rm = TRUE)[[1]] * 0.05

# create problem
p1 <-
  problem(wa_pu, features = wa_features) %>%
  add_min_shortfall_objective(budget) %>%
  add_relative_targets(0.2) %>%
  add_binary_decisions() %>%
  add_default_solver(gap = 0.1, verbose = FALSE)

# print problem
print(p1)

# solve the problem
s1 <- solve(p1)

# extract the objective
print(attr(s1, "objective"))

# extract state message from the solver
print(attr(s1, "status"))

# plot the solution
plot(s1, main = "Solution", axes = FALSE)

# calculate number of selected planning units by solution
eval_n_summary(p1, s1)

# calculate total cost of solution
eval_cost_summary(p1, s1)

# calculate target coverage for the solution
p1_target_coverage <- eval_target_coverage_summary(p1, s1)
print(p1_target_coverage)

# check percentage of the features that have their target met given the solution
print(mean(p1_target_coverage$met) * 100)

# import locked in data
wa_locked_in <- get_wa_locked_in()

# print data
print(wa_locked_in)

# plot data
plot(wa_locked_in, main = "Existing protected areas", axes = FALSE)

# create new problem with locked in constraints added to it
p2 <-
  p1 %>%
  add_locked_in_constraints(wa_locked_in)

# solve the problem
s2 <- solve(p2)

# plot the solution
plot(s2, main = "Solution", axes = FALSE)

# import locked out data
wa_locked_out <- get_wa_locked_out()

# print data
print(wa_locked_out)

# plot data
plot(wa_locked_out, main = "Areas not available for protection", axes = FALSE)

# create new problem with locked out constraints added to it
p3 <-
  p2 %>%
  add_locked_out_constraints(wa_locked_out)

# solve the problem
s3 <- solve(p3)

# plot the solution
plot(s3, main = "Solution", axes = FALSE)

# create new problem with boundary penalties added to it
p4 <-
  p3 %>%
  add_boundary_penalties(penalty = 0.003, edge_factor = 0.5)

# solve the problem
s4 <- solve(p4)

# plot the solution
plot(s4, main = "Solution", axes = FALSE)

# calculate importance scores
rc <-
  p4 %>%
  eval_ferrier_importance(s4)

# print scores
print(rc)

# plot the total importance scores
## note that gray cells are not selected by the prioritization
plot(
  rc[["total"]], main = "Importance scores", axes = FALSE,
  breaks = c(0, 1e-10, 0.005, 0.01, 0.025),
  col = c("#e5e5e5", "#fff7ec", "#fc8d59", "#7f0000")
)

# More resources:
# - Connectivity: https://prioritizr.net/articles/connectivity_tutorial.html
# - Management zones: https://prioritizr.net/articles/management_zones_tutorial.html
# - Trade-offs: https://prioritizr.net/articles/calibrating_trade-offs_tutorial.html