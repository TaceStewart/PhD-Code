# Made by Charlotte for Tace, 2024 #

# Load libraries
library(ggplot2)
library(dplyr)
library(terra)
library(purrr)
library(viridis)
library(spatstat)
library(ggpubr)
library(raster)

# The RISDM package must be downloaded from github
#  - see https://github.com/Scott-Foster/RISDM
# install.packages('devtools')
# library(devtools)
# install.packages("INLA",
#                  repos=c(getOption("repos"),  
#                          INLA="https://inla.r-inla-download.org/R/stable"), 
#                  dep=TRUE)
# devtools::install_github(repo="Scott-Foster/RISDM", 
#                          build_vignettes=FALSE)
library(RISDM) 

# DOMAIN SETUP ------------------------------------------------------------
# This is the domain setup for the simulated landscape.

# START with set up of resolution and north/east step length for later Site A 
# and B grid creation.
ncol <- 1000
nrow <- 1000
res <- 1

east_min <- 0
east_max <- 1000
north_min <- 0
north_max <- 1000

# We generate the grid resolution from min, max dimensions 
# and the number of pixels
n_bau_east <- ncol
n_bau_north <- nrow
# so now we have n_bau_east x n_bau_north grid cells

# Obtain the cell resolution
bau_east_step <- (east_max - east_min) / n_bau_east
bau_north_step <- (north_max - north_min) / n_bau_north 

# Generate grid centroid coordinates
# We do this so that our centroid begins in the centre of a cell 
# (hence, bau_east_step/2))
eastings <- seq(east_min + bau_east_step/2, #sequence from
                east_max - bau_east_step/2, #to
                by = bau_east_step)         #by
northings <- seq(north_min + bau_north_step/2, 
                 north_max - bau_north_step/2, 
                 by = bau_north_step)

# Create a data frame of all possible coordinates
coords <- as.matrix(expand.grid(eastings, northings)) 
colnames(coords) <- c("eastings", "northings")

# Run setup values for simulations 
variance <- 0.5 # Variance of the Gaussian field at distance zero (changed  from 0.5)
scal <- 20 # Range of the Gaussian field 

# Number of replicates per range
nreps <- 3

# 1a. Simulate spatially-varying covariate -----------------------------------
# I’ve included here the option to simulate a spatially-varying covariate, 
# or a linear covariate. 
# A spatially-varying covariate has spatial clustering that is controlled with 
# the parameter ‘rho’. Smaller rho = more clustered, larger = less clustered.

landscape.rast <- terra::rast(xmin = east_min, 
                              xmax = east_max, 
                              ymin = north_min,  
                              ymax = north_max, 
                              nrows = nrow,
                              ncols = ncol,
                              vals = 1:1000)

crs(landscape.rast) <- "epsg:3857" # Setting to WGS 84 / Pseudo-Mercator projection for later functions requiring cell size

xSeq <- terra::xFromCol(landscape.rast)
ySeq <- terra::yFromRow(landscape.rast)

# Simulate a covariate
cov1 <- RISDM:::fftGPsim2( x = xSeq, y = ySeq, 
                           sig2 = 1 , rho = 100, 
                           nu = 1/2) %>% 
  rast()
crs(cov1) <- "epsg:3857" # Setting to WGS 84 / Pseudo-Mercator projection for later functions requiring cell size
names(cov1) <- "cov"

# Get coords of original raster
coords <- xyFromCell(cov1, 1:ncell(cov1))

# Convert raster to matrix object
cov1.df <- as.data.frame(cov1, xy = T)
cov1.mat <- as.matrix(cov1.df)
colnames(cov1.df) <- c("x", "y", "cov")
colnames(cov1.mat) <- c("x", "y", "cov")

# Simulate a covariate
cov2 <- RISDM:::fftGPsim2( x=xSeq, y=ySeq, 
                           sig2 = 1 , rho = 10, nu = 1/2) %>% 
  rast()
crs(cov2) <- "epsg:3857" # Setting to WGS 84 / Pseudo-Mercator projection for later functions requiring cell size
names(cov2) <- "cov"

# Get coords of original raster
coords <- xyFromCell(cov2, 1:ncell(cov2))

# Convert raster to matrix object
cov2.df <- as.data.frame(cov2, xy = T)
cov2.mat <- as.matrix(cov2.df)
colnames(cov2.df) <- c("x", "y", "cov")
colnames(cov2.mat) <- c("x", "y", "cov")

# 1b. Simulate a linear covariate --------------------------------------------

# Generate a matrix of continuous values from 0 to 1, going left to right
covariate_matrix <- matrix(seq(1, 0, length.out = ncol), 
                           nrow = nrow, ncol = ncol, 
                           byrow = TRUE)

# Flatten the matrix into a vector to match the expected input format for 'vals'
covariate_vals <- as.vector(covariate_matrix)

cov3 <- rast(nrows = nrow,
             ncols = ncol,
             xmin = east_min,
             xmax = east_max,
             ymin = north_min,
             ymax = north_max,
             resolution = res,
             vals = covariate_vals,
             names = c("cov")
)
crs(cov3) <- "epsg:3857" # Setting to WGS 84 / Pseudo-Mercator projection for later functions requiring cell size

# Get coords of original raster
coords <- xyFromCell(cov3, 1:ncell(cov3))

# Convert raster to matrix object
cov3.df <- as.data.frame(cov3, xy = T)
cov3.mat <- as.matrix(cov3.df)
colnames(cov3.df) <- c("x", "y", "cov")
colnames(cov3.mat) <- c("x", "y", "cov")

# PLOT ----------------------------------------------------------------------
# Covariate 1, rho = 100
p1 <- cov1 %>% 
  as.data.frame(xy = T) %>%  
  ggplot() + 
  geom_tile(aes(x = x, y = y, fill = cov)) + 
  scale_fill_viridis() +
  coord_fixed() + 
  theme_bw() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        legend.title = element_blank()) +
  ggtitle('Covariate 1 (e.g. temperature)')

# Covariate 2, rho = 10
p2 <- cov2 %>%
  as.data.frame(xy = T) %>%  
  ggplot() + 
  geom_tile(aes(x = x, y = y, fill = cov)) + 
  scale_fill_viridis() +
  coord_fixed() + 
  theme_bw() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        legend.title = element_blank()) +
  ggtitle('Covariate 2 (e.g. precipitation)')

# Covariate 3, linear
p3 <- cov3 %>% 
  as.data.frame(xy = T) %>%  
  ggplot() + 
  geom_tile(aes(x = x, y = y, fill = cov)) + 
  scale_fill_viridis() +
  coord_fixed() + 
  theme_bw() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        legend.title = element_blank()) +
  ggtitle('Covariate 3 (e.g. Elevation)')

# Combine plots
ggarrange(p1, p2, p3, ncol = 3)

# Save plot
ggsave("covariates.png", width = 10, height = 5, dpi = 300)

# 2. Simulate latent spp. distribution --------------------------------------

# Here, what you’re doing is simulating a latent spp. distribution across the 
# landscape. This is the ‘true’ distribution of the species and is a Poisson 
# Point Process with a mean intensity mu that is just a function of the 
# intercept beta0 and the coefficients beta1 and beta2.

# When you run the rpoispp() function you’re drawing a realisation from the 
# Poisson Point Process and you can say that these points are your individuals.
beta0 <- -2  # Intercept
beta1 <- 2   # Coefficient for cov 1
beta2 <- 0.1 # Coefficient for cov 2
beta3 <- 0.5 # Coefficient for cov 3

# Print the expected mean intensity (assuming mean of covariate is 0)
exp(beta0 + beta1*(0) + beta2*(0) + beta3*(0))

# Calculate the fixed effect
fe <- beta0 + beta1*cov1.mat[, "cov"] + beta2*cov2.mat[, "cov"] + beta3*cov3.mat[, "cov"]

# Make the mean 
mu <- cov1.df %>% mutate(cov = fe)
mu <- spatstat.geom::as.im(mu)

# Create a Poisson Point Process with your mu
ipp <- rpoispp(exp(mu))

spp_process <- cbind(x = ipp$x, y = ipp$y)

# If you want less points, you can try changing the intercept to a smaller
# value e.g. -3 or -4.


# You can also thin your Poisson Point Process by a probability which would 
# simulate random sampling of species.
# detect.prob = 0.01

# Thin the process by the probability
# thin <- cbind(spp_process, 
              # presence = rbinom(nrow(spp_process), 1, detect.prob))

# thin <- thin[thin[, "presence"] == 1, ]

# In this plot black is the original species occurrences and red is the 
# thinned species occurrences.
cov1 %>% 
  as.data.frame(xy = T) %>% 
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = cov)) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 0.5)) +
  coord_fixed() +
  geom_point(data = spp_process, 
             aes(x = x, y = y), 
             color = "black", size = 0.5, alpha = 0.5) +
  # geom_point(data = thin, aes(x = x, y = y), 
  #            color = "red", size = 0.5) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        legend.title = element_blank(),
        plot.margin = unit(c(0.5, 0.1, 0.5, 0.1), "lines"),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle('Species Occurrences')

# Save plot
ggsave("species_occurrences.png", width = 5, height = 5, dpi = 300)

# Merge covariates data frames and species presence
covs <- merge(cov1.df, cov2.df, by = c("x", "y")) %>%
  merge(cov3.df, by = c("x", "y"))
colnames(covs) <- c("x", "y", "cov1", "cov2", "cov3")

# Add species presence with location rounded to nearest 0.5
covs$species_present <- 0
covs$species_present[match(paste(covs$x, covs$y), 
                           paste(ceiling(spp_process[,"x"]*2)/2, 
                                 ceiling(spp_process[,"y"]*2)/2))] <- 1

# Save the data frame
write.csv(covs, "cov_species_data.csv", row.names = FALSE)
