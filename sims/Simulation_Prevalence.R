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
kernel_params <- list(sigma_t = 0.5, sigma_s = 1, ell_t = 25, ell_s = 1)

# Generate synthetic spatio-temporal prevalence data
simulated_data <- generate_spatiotemporal_prevalence(n_locations = 5, n_time_points = 500, kernel_params)

# Probit transform of the simulated data
simulated_data$transformed_prevalence <- pnorm(simulated_data$prevalence)

# Example: Time series plot for the closest location to (0.5, 0.5) and contour plot for time t = 1

target_time_point <- 10
plot_spatiotemporal_prevalence(simulated_data, target_time_point)



location_point <- c(0.5,0.5)
closest_location <- find_closest_location(simulated_data, location_point)

# Plot the time series for the closest location
plot_time_series(simulated_data, closest_location)

#Find the prevalence value in region u_i in time t_i
subset_data <- simulated_data[simulated_data$t == target_time_point & 
                                simulated_data$u1 == closest_location[1] &
                                simulated_data$u2 == closest_location[2],]

prevalence_at_target <- subset_data$transformed_prevalence

print(paste("Closest location to target location", paste(location_point, collapse = ", "), 
            "is", paste(closest_location, collapse = ", ")))
print(paste("Prevalence at closest location and time point", target_time_point, ":", prevalence_at_target))

# Stop the parallel backend
stopCluster(cl)
