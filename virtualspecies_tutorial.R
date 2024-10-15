# Tutorial at https://borisleroy.com/files/virtualspecies-tutorial.html

# Install and load the virtualspecies package
#install.packages("virtualspecies")
#install.packages("geodata")
library(virtualspecies)
library(raster)
library(geodata) # for worldclim as getData() is removed

# 1. Input data
# Load WorldClim (www.worldclim.org) gridded climate data
worldclim <- worldclim_global(var = "bio", 
                              res = 10,
                              path=tempdir())
worldclim

# Layers bio1 and bio12 (annual mean temperature and annual precipitation)
worldclim[[c("wc2.1_10m_bio_1", "wc2.1_10m_bio_12")]]

# Plotting bio1 and bio12
par(oma = c(0.1, 0.1, 0.1, 2.1))
plot(worldclim[[c("wc2.1_10m_bio_1", "wc2.1_10m_bio_12")]])
# Fig. 1.1 Variables bio1 (annual mean temperature in °C * 10) and bio2 (annual precipitation)

# Rename the 19 bio variables for easier use
names(worldclim) <- c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", "bio9", "bio10",
                      "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19")

# 2. First approach: generate virtual species distributions by defining response functions
# Suitability of the environment for bio1 = 15 °C
dnorm(x = 150, mean = 250, sd = 50)

# provide to the helper function formatFunctions which responses you want for which variables
my.parameters <- formatFunctions(bio1 = c(fun = 'dnorm', mean = 250, sd = 50),
                                 bio12 = c(fun = 'dnorm', mean = 4000, sd = 2000))

# Generate a virtual species distribution
my.first.species <- generateSpFromFun(raster.stack = worldclim[[c("bio1", "bio12")]],
                                      parameters = my.parameters,
                                      plot = TRUE)
my.first.species
# Fig. 2.2 Environmental suitability of the generated virtual species

# 2.2 Combine response functions
my.parameters <- formatFunctions(bio1 = c(fun = 'dnorm', mean = 250, sd = 50),
                                 bio12 = c(fun = 'dnorm', mean = 4000, sd = 2000))

# Generation of the virtual species
new.species <- generateSpFromFun(raster.stack = worldclim[[c("bio1", "bio12")]],
                                 parameters = my.parameters,
                                 formula = "2 * bio1 + bio12",
                                 plot = TRUE)
new.species
# Fig. 2.3 Environmental suitability of the generated virtual species

# One can even make complex interactions between partial responses:
my.parameters <- formatFunctions(bio1 = c(fun = 'dnorm', mean = 250, sd = 50),
                                 bio12 = c(fun = 'dnorm', mean = 4000, sd = 2000))

# Generation of the virtual species
new.species <- generateSpFromFun(raster.stack = worldclim[[c("bio1", "bio12")]],
                                 parameters = my.parameters,
                                 formula = "3.1 * bio1^2 - 1.4 * sqrt(bio12) * bio1",
                                 plot = TRUE)
new.species
# Fig. 2.4 Environmental suitability of the generated virtual species

# 3. Second approach: generate virtual species with a Principal Components Analysis
my.stack <- worldclim[[c("bio2", "bio5", "bio6", "bio12", "bio13", "bio14")]]
my.pca.species <- generateSpFromPCA(raster.stack = my.stack)
my.pca.species

# A safe run of the function
my.pca.species <- generateSpFromPCA(raster.stack = my.stack, 
                                    sample.points = TRUE, nb.points = 5000)
my.pca.species

plotResponse(my.pca.species)
# Fig. 3.1 PCA used to generate the virtual species

plot(my.pca.species)
# Fig. 3.2 Environmental suitability of a species generated with a PCA approach