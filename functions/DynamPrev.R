library(spBayes)
library(MASS)
library(tidyverse)
library(foreach)
library(doParallel)


# Create and register a parallel cluster
corenumb=detectCores() - 1
cl <- parallel::makeCluster(corenumb)
doSNOW::registerDoSNOW(cl)

# Function to generate synthetic spatio-temporal prevalence data
generate_spatiotemporal_prevalence <- function(n_locations, n_time_points, kernel_params) {
  # Generate synthetic temporal data
  temporal_data <- seq(1, n_time_points)
  
  # Generate synthetic spatial coordinates
  spatial_coordinates <- matrix(runif(2 * n_locations), ncol = 2)
  
  # Create a mesh of spatio-temporal coordinates
  mesh <- expand.grid(t = temporal_data, u1 = spatial_coordinates[, 1], u2 = spatial_coordinates[, 2])
  n_mesh = nrow(mesh)
  
  # Function to define a spatio-temporal Matérn kernel
  
  spatiotemporal_matern_kernel <- function(t1, u1, t2, u2, sigma_t, sigma_s, ell_t, ell_s) {
    temporal_component <- sigma_t^2 * exp(-((t1 - t2)^2) / (2 * ell_t^2))
    spatial_component <- sigma_s^2 * exp(-sum((u1 - u2)^2) / (2 * ell_s^2))
    return(temporal_component * spatial_component)
  }
  
  # Calculate the spatio-temporal covariance matrix using foreach
  cov_matrix <- foreach(i = 1:nrow(mesh), .combine = "cbind") %dopar% {
    sapply(1:nrow(mesh), function(j) {
      spatiotemporal_matern_kernel(mesh$t[i], c(mesh$u1[i], mesh$u2[i]),
                                   mesh$t[j], c(mesh$u1[j], mesh$u2[j]), 
                                   kernel_params$sigma_t, kernel_params$sigma_s,
                                   kernel_params$ell_t, kernel_params$ell_s)
    })
  }
  
  
  # Sample from the multivariate normal distribution
  simulated_data <- mvrnorm(n = 1, mu = rep(-2, n_mesh), Sigma = cov_matrix)
  
  # Return the simulated data along with the spatio-temporal coordinates
  return(data.frame(mesh, prevalence = simulated_data))
}

set.seed(100)

# Parameters of the spatio-temporal Matérn kernel (1,1,50,1),(1,1,25,1)?
kernel_params <- list(sigma_t = 0.5, sigma_s = 1, ell_t = 25, ell_s = 1)

# Generate synthetic spatio-temporal prevalence data
simulated_data <- generate_spatiotemporal_prevalence(n_locations = 5, n_time_points = 500, kernel_params)

# Stop the parallel backend
stopCluster(cl)

# Probit transform of the simulated data
simulated_data$transformed_prevalence <- pnorm(simulated_data$prevalence)

# Functions to plot the prevalence at time t_i and a time series of prevalence in region u_i

plot_spatiotemporal_prevalence <- function(data, time_point) {
  ggplot(data[data$t == time_point, ], aes(x = u1, y = u2, z = transformed_prevalence)) +
    geom_contour_filled() +
    labs(title = paste("Spatio-temporal Prevalence at Time Point", time_point),
         x = "Spatial Coordinate 1", y = "Spatial Coordinate 2", z = "Transformed Prevalence")
}

plot_time_series <- function(data, location) {
  ggplot(data[data$u1 == location[1] & data$u2 == location[2], ], aes(x = t, y = transformed_prevalence)) +
    geom_line() +
    ylim(0,0.4) +
    labs(title = paste("Time Series Plot at Location", paste(location, collapse = ", ")),
         x = "Time", y = "Transformed Prevalence")
}

# Function to find the closest location in the data to the specified target location
find_closest_location <- function(data, target_location) {
  distances <- sqrt((data$u1 - target_location[1])^2 + (data$u2 - target_location[2])^2)
  closest_index <- which.min(distances)
  closest_location <- c(data$u1[closest_index], data$u2[closest_index])
  return(closest_location)
}

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

unique(simulated_data$u1)
unique(simulated_data$u2)
gr = expand.grid(simulated_data$u1,simulated_data$u2)

unique(gr)
