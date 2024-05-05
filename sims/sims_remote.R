# Dependencies
library(tidyverse)

source("functions/tests.R")
source("functions/costs.R")


# Simulation prevalence
prev = seq(0,0.35,0.01)

# Calculate the number of tests
tests = lapply(X = prev, calculate_tests, n = 1000, sims = 0)

# Calculating economic costs

# Paramater values
n = 1000
cv = 1000
cm = 25
cp = 50
cl = 25
tau0 = 750

tau = lapply(tests, function(mat) mat[, "Theoretical"])
omega = lapply(tests, function(mat) mat[, "Duration"])

mu = 50
co = 150

h_values = seq(0,1,by = 0.2)

# Define a list to store the results for each h
economic_costs_list = list()

# Iterate over h values
for (h in h_values) {
  # Define a list to store the results for each time point
  result_costs_list = list()

  
  # Iterate over the indices
  for (i in seq_along(tau)) {
    # Extract tau and omega for the current time point
    current_tau = tau[[i]]
    current_omega = omega[[i]]
    

    
    # Call calculateEconomicCosts for the current time point and h value
    current_costs = calculateEconomicCosts(cv, cm, cp, cl, current_tau, tau0, h, current_omega, n, mu, co)
    
    # Store the result in the list
    result_costs_list[[i]] = current_costs
  }
  
  # Store the results for this h value
  economic_costs_list[[as.character(h)]] = result_costs_list

}

# Combine the results for different h values
result_costs = do.call(rbind, lapply(names(economic_costs_list), function(h) {
  data.frame(
    Time = rep(prev, each = nrow(economic_costs_list[[h]][[1]])),
    Algorithm = rep(economic_costs_list[[h]][[1]][, "Algorithm"], times = length(economic_costs_list[[h]])),
    DC = unlist(lapply(economic_costs_list[[h]], function(result) result[, "DC"])),
    CS = unlist(lapply(economic_costs_list[[h]], function(result) result[, "CS"])),
    Costs = unlist(lapply(economic_costs_list[[h]], function(result) result[, "Costs"])),
    h = rep(as.numeric(h), each = nrow(economic_costs_list[[h]][[1]]))
  )
}))


# Filter the data to keep only the lowest cost line for each facet
lowest_costs = result_costs %>%
  group_by(h, Time) %>%
  filter(Costs == min(Costs, na.rm = TRUE)) %>%
  arrange("Individual") %>%  
  slice(1) %>%
  ungroup()

# Plotting with facet_wrap
ggplot(result_costs, aes(x = Time, y = Costs, color = Algorithm)) +
  geom_line(aes(group = Algorithm), linewidth = 1, alpha = 0.1) +  
  geom_line(data = lowest_costs, aes(group = 1), size = 1) +
  facet_wrap(~ h, nrow = 3, ncol = 2, scales = "free_y", labeller = label_both) +
  labs(title = "Progress of economic cost per individual for different prevalence values",
       x = "Prevalence",
       y = "Cost per individual") +
  theme_bw() +
  theme(legend.position = "right",
        legend.key.size = unit(3, "lines"))




