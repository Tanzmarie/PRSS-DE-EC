# Dependencies
library(tidyverse)

source("functions/Tests.R")
source("functions/Costs.R")


# Simulation prevalence
prev = seq(0,0.35,0.01)

# Calculate the number of tests
tests = lapply(X = prev, calculate_tests, n = 1000, sims = 100)

# Calculating economic costs

# Paramater values
n = 1000
cv = 1000
cm = 25
cp = 50
cl = 25
tau0 = 750

tau <- lapply(tests, function(mat) mat[, "Tests"])
ltau = lapply(tests, function(mat) mat[, "Lower"])
utau = lapply(tests, function(mat) mat[, "Upper"])
omega <- lapply(tests, function(mat) mat[, "Duration"])
k = unlist(round(n * prev))

mu = 30
co = 150

h_values <- seq(0,1,by = 0.1)

# Define a list to store the results for each h
economic_costs_list <- list()
l_economic_costs_list <- list()
u_economic_costs_list <- list()

# Iterate over h values
for (h in h_values) {
  # Define a list to store the results for each time point
  result_costs_list <- list()
  result_costs_list2 <- list()
  result_costs_list3 <- list()
  
  
  # Iterate over the indices
  for (i in seq_along(tau)) {
    # Extract tau and omega for the current time point
    current_tau <- tau[[i]]
    l_current_tau <- ltau[[i]]
    u_current_tau <- utau[[i]]
    current_omega <- omega[[i]]
    
    # Calculate k based on your data (adjust as needed)
    k_cur <- k[i]
    
    # Call calculateEconomicCosts for the current time point and h value
    current_costs <- calculateEconomicCosts(cv, cm, cp, cl, current_tau, tau0, h, current_omega, n, mu, k_cur, co)
    l_current_costs <- calculateEconomicCosts(cv, cm, cp, cl, l_current_tau, tau0, h, current_omega, n, mu, k_cur, co)
    u_current_costs <- calculateEconomicCosts(cv, cm, cp, cl, u_current_tau, tau0, h, current_omega, n, mu, k_cur, co)
    
    # Store the result in the list
    result_costs_list[[i]] <- current_costs
    result_costs_list2[[i]] <- l_current_costs
    result_costs_list3[[i]] <- u_current_costs
  }
  
  # Store the results for this h value
  economic_costs_list[[as.character(h)]] <- result_costs_list
  l_economic_costs_list[[as.character(h)]] <- result_costs_list2
  u_economic_costs_list[[as.character(h)]] <- result_costs_list3
}

# Combine the results for different h values
result_costs <- do.call(rbind, lapply(names(economic_costs_list), function(h) {
  data.frame(
    Time = rep(prev, each = nrow(economic_costs_list[[h]][[1]])),
    Algorithm = rep(economic_costs_list[[h]][[1]][, "Algorithm"], times = length(economic_costs_list[[h]])),
    DC = unlist(lapply(economic_costs_list[[h]], function(result) result[, "DC"])),
    CS = unlist(lapply(economic_costs_list[[h]], function(result) result[, "CS"])),
    Costs = unlist(lapply(economic_costs_list[[h]], function(result) result[, "Costs"])),
    h = rep(as.numeric(h), each = nrow(economic_costs_list[[h]][[1]]))
  )
}))


result_costs2 <- do.call(rbind, lapply(names(l_economic_costs_list), function(h) {
  data.frame(
    Time = rep(prev, each = nrow(l_economic_costs_list[[h]][[1]])),
    Algorithm = rep(l_economic_costs_list[[h]][[1]][, "Algorithm"], times = length(l_economic_costs_list[[h]])),
    DC = unlist(lapply(l_economic_costs_list[[h]], function(result) result[, "DC"])),
    CS = unlist(lapply(l_economic_costs_list[[h]], function(result) result[, "CS"])),
    Costs = unlist(lapply(l_economic_costs_list[[h]], function(result) result[, "Costs"])),
    h = rep(as.numeric(h), each = nrow(l_economic_costs_list[[h]][[1]]))
  )
}))

result_costs3 <- do.call(rbind, lapply(names(u_economic_costs_list), function(h) {
  data.frame(
    Time = rep(prev, each = nrow(u_economic_costs_list[[h]][[1]])),
    Algorithm = rep(u_economic_costs_list[[h]][[1]][, "Algorithm"], times = length(u_economic_costs_list[[h]])),
    DC = unlist(lapply(u_economic_costs_list[[h]], function(result) result[, "DC"])),
    CS = unlist(lapply(u_economic_costs_list[[h]], function(result) result[, "CS"])),
    Costs = unlist(lapply(u_economic_costs_list[[h]], function(result) result[, "Costs"])),
    h = rep(as.numeric(h), each = nrow(u_economic_costs_list[[h]][[1]]))
  )
}))

result_costs$Lower = result_costs2$Costs
result_costs$Upper = result_costs3$Costs


# Filter the data to keep only the lowest cost line for each facet
lowest_costs <- result_costs %>%
  group_by(h, Time) %>%
  filter(Costs == min(Costs, na.rm = TRUE)) %>%
  arrange("Individual") %>%  # Replace priority_variable with the variable you want to prioritize
  slice(1) %>%
  ungroup()



# Plotting with facet_wrap
ggplot(result_costs, aes(x = Time, y = Costs, color = Algorithm)) +
  geom_line(aes(group = Algorithm), size = 1, alpha = 0.1) + 
  geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = Algorithm), alpha = 0.1) + 
  # geom_line(data = lowest_costs, aes(group = 1), size = 1) +
  facet_wrap(~ h, nrow = 3, ncol = 4, scales = "free_y", 
             labeller = labeller(h = function(value) paste0("h = ", value))) +
  labs(title = "Evolution of economic cost for different prevalence values",
       x = "Prevalence",
       y = "Cost") +
  ylim(0,150000) +
  theme_bw() +
  theme(legend.position = "right",
        legend.key.size = unit(3, "lines"))





