# Dependencies
library(tidyverse)

source("functions/DynamPrev.R")
source("functions/Tests.R")
source("functions/Costs.R")

# Load data for the epidemics

load("prevalence.RData")

# Find the prevalence value in region u_i in time t_i
location_point <- c(0.5,0.5)

closest_location <- find_closest_location(simulated_data, location_point)

subset_data = simulated_data[simulated_data$u1 == closest_location[1] &
                               simulated_data$u2 == closest_location[2],]

# Calculate the number of tests
tests = lapply(X = unlist(subset_data[5]), calculate_tests, n = 1000)

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
res <- exp(c(3.34, 3.49, 3.64))

# Costs
n <- 1000
cv <- 1000
cm <- 25
cp = 50
cl <- 25
tau0 <- 850
h = 0.5
co = 150

tau <- lapply(tests, function(mat) mat[, "Tests"])
omega <- lapply(tests, function(mat) mat[, "Waiting.Times"])
k <- unlist(round(n * subset_data[5]))

# Define a list to store the results for each mu
economic_costs_list <- list()

# Iterate over mu values
for (mu_value in res) {
  # Define a list to store the results for each time point
  result_costs_list <- list()
  
  # Iterate over the indices
  for (i in seq_along(tau)) {
    # Extract tau and omega for the current time point
    current_tau <- tau[[i]]
    current_omega <- omega[[i]]
    
    # Calculate k based on your data (adjust as needed)
    k_cur <- k[i]
    
    # Call calculateEconomicCosts for the current time point, fixed h value, and varied mu
    current_costs <- calculateEconomicCosts(cv, cm, cp, cl, current_tau, tau0, h, current_omega, n, mu_value, k_cur, co)
    
    # Store the result in the list
    result_costs_list[[i]] <- current_costs
  }
  
  # Store the results for this mu value
  economic_costs_list[[as.character(mu_value)]] <- result_costs_list
}

# Combine the results for different mu values
result_costs <- do.call(rbind, lapply(names(economic_costs_list), function(mu) {
  data.frame(
    Time = rep(seq_along(economic_costs_list[[mu]]), each = nrow(economic_costs_list[[mu]][[1]])),
    Algorithm = rep(economic_costs_list[[mu]][[1]][, "Algorithm"], times = length(economic_costs_list[[mu]])),
    DC = unlist(lapply(economic_costs_list[[mu]], function(result) result[, "DC"])),
    CS = unlist(lapply(economic_costs_list[[mu]], function(result) result[, "CS"])),
    Costs = unlist(lapply(economic_costs_list[[mu]], function(result) result[, "Costs"])),
    h = rep(h, each = nrow(economic_costs_list[[mu]][[1]])),
    MU = rep(as.numeric(mu), each = nrow(economic_costs_list[[mu]][[1]]))
  )
}))

# Plotting with facet_grid
x11()
ggplot(result_costs, aes(x = Time, y = Costs, color = Algorithm)) +
  geom_line(aes(group = Algorithm), linewidth = 1, alpha = 0.5) +
  facet_wrap(~ MU, nrow = 3, ncol = 1, scales = "free_y", labeller = label_both) +
  labs(title = "Evolution of Costs Over Time",
       x = "Time",
       y = "Costs") +
  ylim(0, 125000) +
  theme_light() +
  theme(legend.position = "right")
