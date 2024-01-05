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
    ylim(0,0.2) +
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

