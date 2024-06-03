library(readr)
library(tidyverse)

source("functions/tests.R")
source("functions/costs.R")

data = read_csv("application/data/COVID-19-Faelle_7-Tage-Inzidenz_Landkreise.csv")
dt = data[which(data$Landkreis_id == "02000"),]

dt$prevalence = ((dt$`Inzidenz_7-Tage`/7) * 14)/100000

# Initialize a list to store the results for each location
n = c(50,100,500,1000)

tests_list <- list()


# Loop over locations
set.seed(444)
for (i in n) {
  
  
  # Calculate the number of tests
  tests <- lapply(X = dt$prevalence, calculate_tests, n = i, sims = 50)
  
  
  
  
  # Store the results in the list
  tests_list[[paste0("n =", i)]] <- list(tests = tests)
}



# Calculating Economic Costs
res = c(80.64042,87.35672,95.58348)

# Paramater values
cf = 1000
cv = 150
tau0 = 750
cl = 150
mu = res[2]
h = 0.5


# Define a list to store the results for each location
economic_costs_list <- list()
l_economic_costs_list = list()
u_economic_costs_list = list()



# Iterate over locations
for (pop in n) {
  # Extract tests and k for the current location
  tests <- tests_list[[paste0("n =", pop)]][["tests"]]
  
  # Define a list to store the results for each time point
  result_costs_list <- list()
  result_costs_list2 = list()
  result_costs_list3 = list()
  
  
  # Iterate over indices
  for (i in seq_along(tests)) {
    # Extract tau and omega for the current time point
    current_tau <- tests[[i]]$Tests
    l_current_tau = tests[[i]]$Lower
    u_current_tau = tests[[i]]$Upper
    current_omega <- tests[[i]]$Duration
    
    
    # Call calculateEconomicCosts for the current time point and fixed h value
    current_costs = calculateEconomicCosts(cf, cv, cl, current_tau, tau0, h, current_omega, pop, mu)
    l_current_costs = calculateEconomicCosts(cf, cv, cl, l_current_tau, tau0, h, current_omega, pop, mu)
    u_current_costs = calculateEconomicCosts(cf, cv, cl, u_current_tau, tau0, h, current_omega, pop, mu)
    
    
    # Store the result in the list
    result_costs_list[[i]] <- current_costs
    result_costs_list2[[i]] = l_current_costs
    result_costs_list3[[i]] = u_current_costs
  }
  
  # Store the results for this h value
  economic_costs_list[[as.character(pop)]] <- result_costs_list
  l_economic_costs_list[[as.character(pop)]] = result_costs_list2
  u_economic_costs_list[[as.character(pop)]] = result_costs_list3
  
}

# Combine the results for different locations
result_costs <- do.call(rbind, lapply(names(economic_costs_list), function(pop) {
  data.frame(
    Time = rep(seq_along(economic_costs_list[[pop]]), each = nrow(economic_costs_list[[pop]][[1]])),
    Algorithm = rep(economic_costs_list[[pop]][[1]][, "Algorithm"], times = length(economic_costs_list[[pop]])),
    DC = unlist(lapply(economic_costs_list[[pop]], function(result) result[, "DC"])),
    CS = unlist(lapply(economic_costs_list[[pop]], function(result) result[, "CS"])),
    Costs = unlist(lapply(economic_costs_list[[pop]], function(result) result[, "Costs"])),
    n = rep(pop, each = nrow(economic_costs_list[[pop]][[1]]))
  )
}))

result_costs2 <- do.call(rbind, lapply(names(l_economic_costs_list), function(pop) {
  data.frame(
    Time = rep(seq_along(l_economic_costs_list[[pop]]), each = nrow(l_economic_costs_list[[pop]][[1]])),
    Algorithm = rep(l_economic_costs_list[[pop]][[1]][, "Algorithm"], times = length(l_economic_costs_list[[pop]])),
    DC = unlist(lapply(l_economic_costs_list[[pop]], function(result) result[, "DC"])),
    CS = unlist(lapply(l_economic_costs_list[[pop]], function(result) result[, "CS"])),
    Costs = unlist(lapply(l_economic_costs_list[[pop]], function(result) result[, "Costs"])),
    n = rep(pop, each = nrow(l_economic_costs_list[[pop]][[1]]))
  )
}))

result_costs3 <- do.call(rbind, lapply(names(u_economic_costs_list), function(pop) {
  data.frame(
    Time = rep(seq_along(u_economic_costs_list[[pop]]), each = nrow(u_economic_costs_list[[pop]][[1]])),
    Algorithm = rep(u_economic_costs_list[[pop]][[1]][, "Algorithm"], times = length(u_economic_costs_list[[pop]])),
    DC = unlist(lapply(u_economic_costs_list[[pop]], function(result) result[, "DC"])),
    CS = unlist(lapply(u_economic_costs_list[[pop]], function(result) result[, "CS"])),
    Costs = unlist(lapply(u_economic_costs_list[[pop]], function(result) result[, "Costs"])),
    n = rep(pop, each = nrow(u_economic_costs_list[[pop]][[1]]))
  )
}))

result_costs$Lower = result_costs2$Costs
result_costs$Upper = result_costs3$Costs

# Filter the cov_dat to keep only the lowest cost line for each facet
lowest_costs <- result_costs %>%
  group_by(n, Time) %>%
  filter(Costs == min(Costs, na.rm = TRUE)) %>%
  arrange("Individual") %>%  
  slice(1) %>%
  ungroup()

lowest_costs2 = result_costs %>%
  group_by(n, Time) %>%
  filter(Lower == min(Lower, na.rm = TRUE)) %>%
  arrange("Individual") %>%  
  slice(1) %>%
  ungroup()

lowest_costs3 = result_costs %>%
  group_by(n, Time) %>%
  filter(Upper == min(Upper, na.rm = TRUE)) %>%
  arrange("Individual") %>%  
  slice(1) %>%
  ungroup()

lowest_costs2$Costs = lowest_costs2$Lower
lowest_costs3$Costs = lowest_costs3$Upper

# Plotting

x11()
algorithm_colors =  c("Individual" = "black",
                      "Dorfman" = "green",
                      "Double-Pooling" = "blue",
                      "R-Pooling" = "cyan2",
                      "3-Stage" = "red",
                      "4-Stage" = "darkgoldenrod"
)

ggplot(result_costs, aes(x = Time, y = Costs, color = Algorithm)) +
  geom_line(data = lowest_costs, aes(group = 1), linewidth = 0.1) +
  geom_line(data = lowest_costs2, aes(group = 1), linewidth = 0.5, alpha = 0.1) +
  geom_line(data = lowest_costs3, aes(group = 1), linewidth = 0.5, alpha = 0.1) +
  facet_wrap(~ n, nrow = 2, ncol = 2, scales = "free_y", 
             labeller = labeller(n = function(value) paste0("n = ", value))) +
  labs(title = "Progress of economic cost per individual for the COVID-19 pandemic",
       x = "Time in days",
       y = "Economic cost per individual") +
  theme_bw() +
  theme(legend.position = "right",
        legend.key.size = unit(3, "lines")) +
  scale_color_manual(values = algorithm_colors)





