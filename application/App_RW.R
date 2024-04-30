library(readr)
library(tidyverse)

source("functions/tests.R")
source("functions/costs.R")

data = read_csv("application/data/COVID-19-Faelle_7-Tage-Inzidenz_Landkreise.csv")
dt = data[which(data$Landkreis_id == "02000"),]


dt$prevalence = ((dt$`Inzidenz_7-Tage`/7) * 14)/100000


tests = lapply(X = dt$prevalence, calculate_tests, n = 1000, sims = 10)


# Calculating Economic Costs
res = c(79.83803,87.35672,95.58348)

# Paramater values
n = 1000
cv = 1000
cm = 25
cp = 50
cl = 25
tau0 = 750

tau = lapply(tests, function(mat) mat[, "Tests"])
ltau = lapply(tests, function(mat) mat[, "Lower"])
utau = lapply(tests, function(mat) mat[, "Upper"])
omega = lapply(tests, function(mat) mat[, "Duration"])
k = unlist(round(n * dt$prevalence))

mu = res[2]
co = 150

h_values = seq(0,1,by = 0.1)

# Define a list to store the results for each h
economic_costs_list = list()
l_economic_costs_list = list()
u_economic_costs_list = list()

# Iterate over h values
for (h in h_values) {
  # Define a list to store the results for each time point
  result_costs_list = list()
  result_costs_list2 = list()
  result_costs_list3 = list()
  
  
  # Iterate over the indices
  for (i in seq_along(tau)) {
    # Extract tau and omega for the current time point
    current_tau = tau[[i]]
    l_current_tau = ltau[[i]]
    u_current_tau = utau[[i]]
    current_omega = omega[[i]]
    
    # Calculate k based on your data (adjust as needed)
    k_cur = k[i]
    
    # Call calculateEconomicCosts for the current time point and h value
    current_costs = calculateEconomicCosts(cv, cm, cp, cl, current_tau, tau0, h, current_omega, n, mu, co)
    l_current_costs = calculateEconomicCosts(cv, cm, cp, cl, l_current_tau, tau0, h, current_omega, n, mu, co)
    u_current_costs = calculateEconomicCosts(cv, cm, cp, cl, u_current_tau, tau0, h, current_omega, n, mu, co)
    
    # Store the result in the list
    result_costs_list[[i]] = current_costs
    result_costs_list2[[i]] = l_current_costs
    result_costs_list3[[i]] = u_current_costs
  }
  
  # Store the results for this h value
  economic_costs_list[[as.character(h)]] = result_costs_list
  l_economic_costs_list[[as.character(h)]] = result_costs_list2
  u_economic_costs_list[[as.character(h)]] = result_costs_list3
}

# Combine the results for different h values
result_costs = do.call(rbind, lapply(names(economic_costs_list), function(h) {
  data.frame(
    Time = rep(seq_along(economic_costs_list[[h]]), each = nrow(economic_costs_list[[h]][[1]])),
    Algorithm = rep(economic_costs_list[[h]][[1]][, "Algorithm"], times = length(economic_costs_list[[h]])),
    DC = unlist(lapply(economic_costs_list[[h]], function(result) result[, "DC"])),
    CS = unlist(lapply(economic_costs_list[[h]], function(result) result[, "CS"])),
    Costs = unlist(lapply(economic_costs_list[[h]], function(result) result[, "Costs"])),
    h = rep(as.numeric(h), each = nrow(economic_costs_list[[h]][[1]]))
  )
}))


result_costs2 = do.call(rbind, lapply(names(l_economic_costs_list), function(h) {
  data.frame(
    Time = rep(seq_along(economic_costs_list[[h]]), each = nrow(l_economic_costs_list[[h]][[1]])),
    Algorithm = rep(l_economic_costs_list[[h]][[1]][, "Algorithm"], times = length(l_economic_costs_list[[h]])),
    DC = unlist(lapply(l_economic_costs_list[[h]], function(result) result[, "DC"])),
    CS = unlist(lapply(l_economic_costs_list[[h]], function(result) result[, "CS"])),
    Costs = unlist(lapply(l_economic_costs_list[[h]], function(result) result[, "Costs"])),
    h = rep(as.numeric(h), each = nrow(l_economic_costs_list[[h]][[1]]))
  )
}))

result_costs3 = do.call(rbind, lapply(names(u_economic_costs_list), function(h) {
  data.frame(
    Time = rep(seq_along(economic_costs_list[[h]]), each = nrow(u_economic_costs_list[[h]][[1]])),
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
lowest_costs = result_costs %>%
  group_by(h, Time) %>%
  filter(Costs == min(Costs, na.rm = TRUE)) %>%
  arrange("Individual") %>%  
  slice(1) %>%
  ungroup()

lowest_costs2 = result_costs %>%
  group_by(h, Time) %>%
  filter(Lower == min(Lower, na.rm = TRUE)) %>%
  arrange("Individual") %>%  
  slice(1) %>%
  ungroup()

lowest_costs3 = result_costs %>%
  group_by(h, Time) %>%
  filter(Upper == min(Upper, na.rm = TRUE)) %>%
  arrange("Individual") %>%  
  slice(1) %>%
  ungroup()



lowest_costs2$Costs = lowest_costs2$Lower
lowest_costs3$Costs = lowest_costs3$Upper

# Plotting with facet_wrap
x11()
ggplot(result_costs, aes(x = Time, y = Costs, color = Algorithm)) +
  geom_line(data = lowest_costs, aes(group = 1), size = 0.1) +
  geom_line(data = lowest_costs2, aes(group = 1), size = 0.5, alpha = 0.5) +
  geom_line(data = lowest_costs3, aes(group = 1), size = 0.5, alpha = 0.5) +
  facet_wrap(~ h, nrow = 3, ncol = 4, scales = "free_y", 
             labeller = labeller(h = function(value) paste0("h = ", value))) +
  labs(title = "Evolution of economic cost per individual for the COVID-19 pandemic",
       x = "Time in days",
       y = "Economic cost per individual") +
  theme_bw() +
  theme(legend.position = "right",
        legend.key.size = unit(3, "lines"))


