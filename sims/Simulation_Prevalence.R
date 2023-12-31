# Dependencies
library(spBayes)
library(MASS)
library(tidyverse)
library(foreach)
library(doParallel)

# Functions works only with parallel backend !
source("functions/DynamPrev.R")

# Create and register a parallel cluster
corenumb=detectCores() - 1
cl <- parallel::makeCluster(corenumb)
doSNOW::registerDoSNOW(cl)

# Generate prevalence data 

set.seed(100)

# Parameters of the spatio-temporal Matérn kernel (1,1,50,1),(1,1,25,1)?
kernel_params <- list(sigma_t = 1, sigma_s = 1, ell_t = 65, ell_s = 1)

# Generate synthetic spatio-temporal prevalence data
simulated_data <- generate_spatiotemporal_prevalence(n_locations = 5, n_time_points = 500, kernel_params)

# Probit transform of the simulated data
simulated_data$transformed_prevalence <- pnorm(simulated_data$prevalence)

# save data

load("prevalence.RData")

# Example: Time series plot for the closest location to (0.5, 0.5) and contour plot for time t = 1

target_time_point <- 500
plot_spatiotemporal_prevalence(simulated_data, target_time_point)

# (0.2,0.8), (0.1,0.2), (0.2,0.1),(1,0.2), (0.7,0.5), (0.7,0.2), (0.1,0.8)
location_point <- c(0.1,0.8)
closest_location <- find_closest_location(simulated_data, location_point)

# Plot the time series for the closest location
plot_time_series(simulated_data, closest_location)


# Stop the parallel backend
stopCluster(cl)
