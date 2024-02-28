library(readr)
library(tidyverse)

source("functions/Tests.R")
source("functions/Costs.R")

data <- read_csv("application/COVID-19-Faelle_7-Tage-Inzidenz_Landkreise.csv")

# location: "09375" == Regensburg

location = unique(data$Landkreis_id)
dt = data[which(data$Landkreis_id == "09375"),]
loc = "09375"


dt$prevalence = ((dt$`Inzidenz_7-Tage`/7) * 14)/100000
dt$prevalence2 = dt$`Faelle_7-Tage`/dt$Bevoelkerung


tests = lapply(X = dt$prevalence, calculate_tests, n = 1000)


# Calculating Economic Costs
# Extract mu values from Simulation of Incomes
res = exp(c(3.34,3.49,3.64))

# Costs
n = 1000
cv = 1000
cm = 25
cp = 50
cl = 25
tau0 = 850

tau <- lapply(tests, function(mat) mat[, "Tests"])
omega <- lapply(tests, function(mat) mat[, "Waiting.Times"])
k = unlist(round(n * dt$prevalence))

mu = res[2]
co = 150

h_values <- seq(0,1,by = 0.1)

# Define a list to store the results for each h
economic_costs_list <- list()

# Iterate over h values
for (h in h_values) {
  # Define a list to store the results for each time point
  result_costs_list <- list()
  
  # Iterate over the indices
  for (i in seq_along(tau)) {
    # Extract tau and omega for the current time point
    current_tau <- tau[[i]]
    current_omega <- omega[[i]]
    
    # Calculate k based on your data (adjust as needed)
    k_cur <- k[i]
    
    # Call calculateEconomicCosts for the current time point and h value
    current_costs <- calculateEconomicCosts(cv, cm, cp, cl, current_tau, tau0, h, current_omega, n, mu, k_cur, co)
    
    # Store the result in the list
    result_costs_list[[i]] <- current_costs
  }
  
  # Store the results for this h value
  economic_costs_list[[as.character(h)]] <- result_costs_list
}

# Combine the results for different h values
result_costs <- do.call(rbind, lapply(names(economic_costs_list), function(h) {
  data.frame(
    Time = rep(seq_along(economic_costs_list[[h]]), each = nrow(economic_costs_list[[h]][[1]])),
    Algorithm = rep(economic_costs_list[[h]][[1]][, "Algorithm"], times = length(economic_costs_list[[h]])),
    DC = unlist(lapply(economic_costs_list[[h]], function(result) result[, "DC"])),
    CS = unlist(lapply(economic_costs_list[[h]], function(result) result[, "CS"])),
    Costs = unlist(lapply(economic_costs_list[[h]], function(result) result[, "Costs"])),
    h = rep(as.numeric(h), each = nrow(economic_costs_list[[h]][[1]]))
  )
}))


# Filter the data to keep only the lowest cost line for each facet
lowest_costs <- result_costs %>%
  group_by(h, Time) %>%
  filter(Costs == min(Costs, na.rm = TRUE)) %>%
  ungroup()

# Plotting with facet_wrap
x11()
ggplot(result_costs, aes(x = Time, y = Costs, color = Algorithm)) +
  geom_point(aes(group = Algorithm), alpha = 0.1, shape = ".") +  
  geom_line(data = lowest_costs, aes(group = 1), size = 1) +  
  facet_wrap(~ h, nrow = 3, ncol = 4, scales = "free_y", 
             labeller = labeller(h = function(value) paste0("h = ", value))) +
  labs(title = "Evolution of Economic Costs over Time",
       x = "Time",
       y = "Costs") +
  ylim(0, 125000) +
  theme_bw() +
  theme(legend.position = "right",
        legend.key.size = unit(3, "lines"))

