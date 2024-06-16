library(readr)
library(tidyverse)
library(furrr)

source("functions/tests.R")
source("functions/costs.R")

data = read_csv("application/data/COVID-19-Faelle_7-Tage-Inzidenz_Landkreise.csv")
dt = data[which(data$Landkreis_id == "02000"),]


dt$prevalence = ((dt$`Inzidenz_7-Tage`/7) * 14)/100000

num_cores = detectCores() - 1
cl = makeCluster(num_cores)
registerDoParallel(cl)
plan(multisession, workers = detectCores() - 1)

tests = future_map(dt$prevalence, calculate_tests, n = 1000, sims = 10)

stopCluster(cl)


# Calculating Economic Costs
res = c(exp(4.39 + (0.98/2)),exp(4.47 + (0.98/2)),exp(4.56 + (0.98/2)))

# Paramater values
n = 1000
cf = 1000
cv = 150
tau0 = 750
cl = 300
mu = res[2]

tau = lapply(tests, function(mat) mat[, "Tests"])
ltau = lapply(tests, function(mat) mat[, "Lower"])
utau = lapply(tests, function(mat) mat[, "Upper"])
omega = lapply(tests, function(mat) mat[, "Duration"])



h_values = c(0,0.4,0.6,0.8,0.9,1)

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
    
    # Call calculateEconomicCosts for the current time point and h value
    current_costs = calculateEconomicCosts(cf, cv, cl, current_tau, tau0, h, current_omega, n, mu)
    l_current_costs = calculateEconomicCosts(cf, cv, cl, l_current_tau, tau0, h, current_omega, n, mu)
    u_current_costs = calculateEconomicCosts(cf, cv, cl, u_current_tau, tau0, h, current_omega, n, mu)
    
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
    Time = rep(seq_along(l_economic_costs_list[[h]]), each = nrow(l_economic_costs_list[[h]][[1]])),
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



# Filter the data to keep only the lowest cost line for each facet
lowest_costs = result_costs %>%
  group_by(h, Time) %>%
  filter(Costs == min(Costs))



lowest_costs2 = result_costs2 %>%
  group_by(h, Time) %>%
  filter(Costs == min(Costs))


lowest_costs3 = result_costs3 %>%
  group_by(h, Time) %>%
  filter(Costs == min(Costs))





# Plotting with facet_wrap
x11()
algorithm_colors =  c("One-stage" = "black",
                      "Two-stage" = "green",
                      "Three-stage" = "blue",
                      "Four-stage" = "darkgoldenrod",
                      "Five-stage" = "red"
)
ggplot(result_costs, aes(x = Time, y = Costs, color = Algorithm)) +
  geom_line(data = lowest_costs, aes(group = 1), linewidth = 0.1) +
  geom_line(data = lowest_costs2, aes(group = 1), linewidth = 0.5, alpha = 0.1) +
  geom_line(data = lowest_costs3, aes(group = 1), linewidth = 0.5, alpha = 0.1) +
  facet_wrap(~ h, nrow = 5, ncol = 3, scales = "free_y", 
             labeller = labeller(h = function(value) paste0("h = ", value))) +
  labs(title = "Progress of economic cost per individual for the COVID-19 pandemic in Hamburg",
       x = "Time in days",
       y = "Economic cost per individual") +
  xlim(0,1500) +
  theme_bw() +
  theme(legend.position = "right",
        legend.key.size = unit(3, "lines")) +
  scale_color_manual(values = algorithm_colors)
  