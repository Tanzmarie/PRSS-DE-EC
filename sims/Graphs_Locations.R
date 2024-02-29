# Dependencies
library(tidyverse)

source("functions/Prevalence.R")
source("functions/Tests.R")
source("functions/Costs.R")

# Load data for the epidemics

load("prevalence.RData")

# Locations for which you want to calculate tests
locations <- cbind(c(0.1, 0.5, 0.1), c(0.8, 0.5, 0.2))

# Initialize a list to store the results for each location
tests_list <- list()

# Loop over locations
for (i in seq_len(nrow(locations))) {
  # Find the closest location
  closest_location <- find_closest_location(simulated_data, locations[i,])
  
  # Subset the data for the closest location
  subset_data <- simulated_data[simulated_data$u1 == closest_location[1] &
                                  simulated_data$u2 == closest_location[2],]
  
  # Calculate the number of tests
  tests <- lapply(X = unlist(subset_data[5]), calculate_tests, n = 1000)
  
  # Calculate k for the current location
  k <- unlist(round(1000 * subset_data[5]))  # Assuming the relevant column is in the same position for each location
  
  
  # Store the results in the list
  tests_list[[paste0("Location_", i)]] <- list(tests = tests, k = k)
}

# Assuming 'result' is the list of matrices you obtained
# Convert the list of matrices to a data frame

result_df <- do.call(rbind, lapply(seq_along(tests), function(i) {
  data.frame(
    Time = i,
    Algorithm = tests[[i]][1],
    Tests = tests[[i]][, "Tests"],
    Negative_Deviation = tests[[i]][, "10% Negative"],
    Positive_Deviation = tests[[i]][, "10% Positive"]
  )
}))

# Plotting
ggplot(result_df, aes(x = Time, y = Tests, color = Algorithm)) +
  geom_line(aes(group = Algorithm), size = 1) +
  geom_ribbon(aes(ymin = Negative_Deviation, ymax = Positive_Deviation, fill = Algorithm), alpha = 0.1) +
  labs(title = "Evolution of Tests Over Time",
       x = "Time",
       y = "Tests") +
  theme_minimal() +
  theme(legend.position = "right")

# Calculating economic costs

# Extract mu values from Simulation of Incomes
res = exp(c(3.34,3.49,3.64))

# Costs
n = 1000
cv = 1000
cm = 25
cp = 50
cl = 25
tau0 = 850
h = 0.5
mu = res[2]
co = 150



# Define a list to store the results for each location
economic_costs_list <- list()

# Iterate over locations
for (loc_index in seq_len(length(tests_list))) {
  # Extract tests and k for the current location
  tests <- tests_list[[paste0("Location_", loc_index)]][["tests"]]
  k <- tests_list[[paste0("Location_", loc_index)]][["k"]]
  
  # Define a list to store the results for each time point
  result_costs_list <- list()
  
  # Iterate over indices
  for (i in seq_along(tests)) {
    # Extract tau and omega for the current time point
    current_tau <- tests[[i]]$Tests
    current_omega <- tests[[i]]$Waiting.Times
    
    # Calculate k based on your data (adjust as needed)
    k_cur <- k[i]
    
    # Call calculateEconomicCosts for the current time point and fixed h value
    current_costs <- calculateEconomicCosts(cv, cm, cp, cl, current_tau, tau0, h, current_omega, n, mu, k_cur, co)
    
    # Store the result in the list
    result_costs_list[[i]] <- current_costs
  }
  
  # Store the results for this h value
  economic_costs_list[[as.character(loc_index)]] <- result_costs_list
  
}

# Combine the results for different locations
result_costs <- do.call(rbind, lapply(names(economic_costs_list), function(loc_index) {
    data.frame(
      Time = rep(seq_along(economic_costs_list[[loc_index]]), each = nrow(economic_costs_list[[loc_index]][[1]])),
      Algorithm = rep(economic_costs_list[[loc_index]][[1]][, "Algorithm"], times = length(economic_costs_list[[loc_index]])),
      DC = unlist(lapply(economic_costs_list[[loc_index]], function(result) result[, "DC"])),
      CS = unlist(lapply(economic_costs_list[[loc_index]], function(result) result[, "CS"])),
      Costs = unlist(lapply(economic_costs_list[[loc_index]], function(result) result[, "Costs"])),
      Location = rep(as.numeric(loc_index), each = nrow(economic_costs_list[[loc_index]][[1]]))
    )
}))

# Filter the data to keep only the lowest cost line for each facet
lowest_costs <- result_costs %>%
  group_by(Location, Time) %>%
  filter(Costs == min(Costs, na.rm = TRUE)) %>%
  ungroup()


# Define coordinates for each location
coordinates <- c("1" = "(0.1, 0.8)", "2" = "(0.5, 0.5)", "3" = "(0.1, 0.2)")

x11()
ggplot(result_costs, aes(x = Time, y = Costs, color = Algorithm)) +
  geom_line(aes(group = Algorithm), size = 1, alpha = 0.1) +  
  geom_line(data = lowest_costs, aes(group = 1), size = 1) +
  facet_wrap(~ Location, nrow = 3, ncol = 4, scales = "free_y", 
             labeller = labeller(Location = function(value) {
               return(coordinates[value])
             })) +
  labs(title = "Evolution of Economic Costs over Time",
       x = "Time",
       y = "Costs") +
  ylim(0, 125000) +
  theme_bw() +
  theme(legend.position = "right",
        legend.key.size = unit(7, "lines"))



