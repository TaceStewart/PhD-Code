# RISDM tutorial found in Scott Foster paper:
# https://nsojournals.onlinelibrary.wiley.com/doi/full/10.1111/ecog.06964

# Install packages if needed  
# library(devtools)
# install.packages("INLA",
#                  repos = c(getOption("repos"),
#                          INLA = "https://inla.r-inla-download.org/R/stable"), 
#                  dep = TRUE)
# devtools::install_github(repo = "Scott-Foster/RISDM", 
#                          build_vignettes=TRUE)

# Load libraries
library(RISDM)
library(terra)

# Load data
# The covariate data are real, and taken as a 1 km grid over the 
# Australian Capital Territory (ACT).
covars <- rast(system.file("extdata", "ACT_DemoData.grd", package = "RISDM"))
names(covars) <- c("lACC", "SMRZ", "TEMP") #short names

# Simulate data confirming to ISDM
# Set seed for reproducible results
dat <- simulateData.isdm(rasterCovars = covars[[c("SMRZ", "TEMP")]], 
                         rasterBiasCovar = covars[["lACC"]],
                         control = list(doPlot = FALSE,
                                        set.random.seed = TRUE,
                                        random.seed = 123456))
