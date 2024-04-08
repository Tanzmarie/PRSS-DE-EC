# Dependencies
library(tidyverse)

source("functions/Tests.R")
source("functions/Costs.R")


# Simulation prevalence
prev = seq(0,0.35,0.01)

# Calculate the number of tests
tests = lapply(X = prev, calculate_tests, n = 1000)

# Calculating economic costs

# Costs
n <- 1000
cv <- 1000
cp <- 50  
cm_values <- c(0, 10, 25, 50, 100, 200)  # Change c_m values
cl <- 25
tau0 <- 750

tau <- lapply(tests, function(mat) mat[, "Tests"])
omega <- lapply(tests, function(mat) mat[, "Duration"])
k <- unlist(round(n * prev))

mu <- 30
co <- 150
h <- 0.5

# Define a list to store the results for each c_m
economic_costs_list <- list()

# Iterate over c_m values
for (cm in cm_values) {
  # Define a list to store the results for each time point
  result_costs_list <- list()
  
  # Iterate over the indices
  for (i in seq_along(tau)) {
    # Extract tau and omega for the current time point
    current_tau <- tau[[i]]
    current_omega <- omega[[i]]
    
    # Calculate k based on your data (adjust as needed)
    k_cur <- k[i]
    
    # Call calculateEconomicCosts for the current time point, fixed h value, and varied c_m
    current_costs <- calculateEconomicCosts(cv, cm, cp, cl, current_tau, tau0, h, current_omega, n, mu, k_cur, co)
    
    # Store the result in the list
    result_costs_list[[i]] <- current_costs
  }
  
  # Store the results for this c_m value
  economic_costs_list[[as.character(cm)]] <- result_costs_list
}

# Combine the results for different c_m values
result_costs <- do.call(rbind, lapply(names(economic_costs_list), function(cm) {
  data.frame(
    Time = rep(prev, each = nrow(economic_costs_list[[cm]][[1]])),
    Algorithm = rep(economic_costs_list[[cm]][[1]][, "Algorithm"], times = length(economic_costs_list[[cm]])),
    DC = unlist(lapply(economic_costs_list[[cm]], function(result) result[, "DC"])),
    CS = unlist(lapply(economic_costs_list[[cm]], function(result) result[, "CS"])),
    Costs = unlist(lapply(economic_costs_list[[cm]], function(result) result[, "Costs"])),
    h = rep(h, each = nrow(economic_costs_list[[cm]][[1]])),
    CM = rep(as.numeric(cm), each = nrow(economic_costs_list[[cm]][[1]]))
  )
}))

# Filter the data to keep only the lowest cost line for each facet
lowest_costs <- result_costs %>%
  group_by(CM, Time) %>%
  filter(Costs == min(Costs, na.rm = TRUE)) %>%
  arrange("Individual") %>%  # Replace priority_variable with the variable you want to prioritize
  slice(1) %>%
  ungroup()



# Plotting with facet_grid
ggplot(result_costs, aes(x = Time, y = Costs, color = Algorithm)) +
  geom_line(aes(group = Algorithm), size = 1, alpha = 0.1) +  
  geom_line(data = lowest_costs, aes(group = 1), size = 1) +
  facet_wrap(~ CM, nrow = 3, ncol = 2, scales = "free_y", labeller = label_both) +
  labs(title = "Evolution of Costs over Time",
       x = "Time",
       y = "Cost") +
  theme_bw() +
  theme(legend.position = "right",
        legend.key.size = unit(3, "lines"))

