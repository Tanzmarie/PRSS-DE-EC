# Dependencies
source("functions/tests.R")
source("functions/costs.R")


# Simulation prevalence
prev = seq(0,0.35,0.01)

# Calculate the number of tests
tests = lapply(X = prev, calculate_tests, n = 1000, sims = 0)

# Calculating economic costs

# Costs
n = 1000
cv = 1000
cm = 25
cp = 50
cl = 25
h = 0.5
co = 150
mu = 50

tau = lapply(tests, function(mat) mat[, "Theoretical"])
omega = lapply(tests, function(mat) mat[, "Duration"])


# Define testing capacity values
tau0_values = c(0, 50, 100, 150, 250, 500, 750, 1000)

# Define a list to store the results for each tau0
economic_costs_list = list()

# Iterate over tau0 values
for (tau0_value in tau0_values) {
  # Define a list to store the results for each time point
  result_costs_list = list()
  
  # Iterate over the indices
  for (i in seq_along(tau)) {
    # Extract tau and omega for the current time point
    current_tau = tau[[i]]
    current_omega = omega[[i]]
    
    
    # Call calculateEconomicCosts for the current time point, fixed h value, fixed mu, and varied tau0
    current_costs = calculateEconomicCosts(cv, cm, cp, cl, current_tau, tau0_value, h, current_omega, n, mu, co)
    
    # Store the result in the list
    result_costs_list[[i]] = current_costs
  }
  
  # Store the results for this tau0 value
  economic_costs_list[[as.character(tau0_value)]] = result_costs_list
}

# Combine the results for different tau0 values
result_costs = do.call(rbind, lapply(names(economic_costs_list), function(tau0) {
  data.frame(
    Time = rep(prev, each = nrow(economic_costs_list[[tau0]][[1]])),
    Algorithm = rep(economic_costs_list[[tau0]][[1]][, "Algorithm"], times = length(economic_costs_list[[tau0]])),
    DC = unlist(lapply(economic_costs_list[[tau0]], function(result) result[, "DC"])),
    CS = unlist(lapply(economic_costs_list[[tau0]], function(result) result[, "CS"])),
    Costs = unlist(lapply(economic_costs_list[[tau0]], function(result) result[, "Costs"])),
    h = rep(h, each = nrow(economic_costs_list[[tau0]][[1]])),
    MU = rep(mu, each = nrow(economic_costs_list[[tau0]][[1]])),
    TAU0 = rep(as.numeric(tau0), each = nrow(economic_costs_list[[tau0]][[1]]))
  )
}))

# Filter the data to keep only the lowest cost line for each facet
lowest_costs = result_costs %>%
  group_by(TAU0, Time) %>%
  filter(Costs == min(Costs, na.rm = TRUE)) %>%
  arrange("Individual") %>%  # Replace priority_variable with the variable you want to prioritize
  slice(1) %>%
  ungroup()


# Plotting with facet_grid
ggplot(result_costs, aes(x = Time, y = Costs, color = Algorithm)) +
  geom_line(aes(group = Algorithm), size = 1, alpha = 0.1) +  
  geom_line(data = lowest_costs, aes(group = 1), size = 1) +
  facet_wrap(~ TAU0, nrow = 4, ncol = 2, scales = "free_y", labeller = label_both) +
  labs(title = "Progression of economic cost per individual for different prevalence values",
       x = "Prevalence",
       y = "Costs per individual") +
  theme_bw() +
  theme(legend.position = "right",
        legend.key.size = unit(3, "lines"))

