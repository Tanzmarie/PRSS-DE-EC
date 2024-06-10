library(readr)
library(tidyverse)

source("functions/tests.R")
source("functions/costs.R")

data <- read_csv("application/data/COVID-19-Faelle_7-Tage-Inzidenz_Landkreise.csv")
dt = data[which(data$Landkreis_id == "02000"),]
dt$prevalence = ((dt$`Inzidenz_7-Tage`/7) * 14)/100000

num_cores = detectCores() - 1
cl = makeCluster(num_cores)
registerDoParallel(cl)
plan(multisession, workers = detectCores() - 1)

tests = future_map(dt$prevalence, calculate_tests, n = 1000, sims = 10)

stopCluster(cl)




# Calculating economic costs
res = c(80.64042,87.35672,95.58348)

# Costs
n = 1000
cf = 1000
cv = 150
tau0 = 750
cl = 300
mu = res[2]
h = 0.5

tau = lapply(tests, function(mat) mat[, "Tests"])
ltau = lapply(tests, function(mat) mat[, "Lower"])
utau = lapply(tests, function(mat) mat[, "Upper"])
omega = lapply(tests, function(mat) mat[, "Duration"])


# Define testing capacity values
tau0_values <- c(0, 50, 100, 250, 500, 1000)

# Define a list to store the results for each h
economic_costs_list = list()
l_economic_costs_list = list()
u_economic_costs_list = list()

# Iterate over h values
for (tau0 in tau0_values) {
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
  economic_costs_list[[as.character(tau0)]] = result_costs_list
  l_economic_costs_list[[as.character(tau0)]] = result_costs_list2
  u_economic_costs_list[[as.character(tau0)]] = result_costs_list3
}

# Combine the results for different h values
result_costs = do.call(rbind, lapply(names(economic_costs_list), function(tau0) {
  data.frame(
    Time = rep(seq_along(economic_costs_list[[tau0]]), each = nrow(economic_costs_list[[tau0]][[1]])),
    Algorithm = rep(economic_costs_list[[tau0]][[1]][, "Algorithm"], times = length(economic_costs_list[[tau0]])),
    DC = unlist(lapply(economic_costs_list[[tau0]], function(result) result[, "DC"])),
    CS = unlist(lapply(economic_costs_list[[tau0]], function(result) result[, "CS"])),
    Costs = unlist(lapply(economic_costs_list[[tau0]], function(result) result[, "Costs"])),
    tau0 = rep(as.numeric(tau0), each = nrow(economic_costs_list[[tau0]][[1]]))
  )
}))


result_costs2 = do.call(rbind, lapply(names(l_economic_costs_list), function(tau0) {
  data.frame(
    Time = rep(seq_along(economic_costs_list[[tau0]]), each = nrow(l_economic_costs_list[[tau0]][[1]])),
    Algorithm = rep(l_economic_costs_list[[tau0]][[1]][, "Algorithm"], times = length(l_economic_costs_list[[tau0]])),
    DC = unlist(lapply(l_economic_costs_list[[tau0]], function(result) result[, "DC"])),
    CS = unlist(lapply(l_economic_costs_list[[tau0]], function(result) result[, "CS"])),
    Costs = unlist(lapply(l_economic_costs_list[[tau0]], function(result) result[, "Costs"])),
    tau0 = rep(as.numeric(tau0), each = nrow(l_economic_costs_list[[tau0]][[1]]))
  )
}))

result_costs3 = do.call(rbind, lapply(names(u_economic_costs_list), function(tau0) {
  data.frame(
    Time = rep(seq_along(economic_costs_list[[tau0]]), each = nrow(u_economic_costs_list[[tau0]][[1]])),
    Algorithm = rep(u_economic_costs_list[[tau0]][[1]][, "Algorithm"], times = length(u_economic_costs_list[[tau0]])),
    DC = unlist(lapply(u_economic_costs_list[[tau0]], function(result) result[, "DC"])),
    CS = unlist(lapply(u_economic_costs_list[[tau0]], function(result) result[, "CS"])),
    Costs = unlist(lapply(u_economic_costs_list[[tau0]], function(result) result[, "Costs"])),
    tau0 = rep(as.numeric(tau0), each = nrow(u_economic_costs_list[[tau0]][[1]]))
  )
}))

result_costs$Lower = result_costs2$Costs
result_costs$Upper = result_costs3$Costs


# Filter the data to keep only the lowest cost line for each facet
lowest_costs = result_costs %>%
  group_by(tau0, Time) %>%
  filter(Costs == min(Costs)) 

lowest_costs2 = result_costs %>%
  group_by(tau0, Time) %>%
  filter(Lower == min(Lower)) 
lowest_costs3 = result_costs %>%
  group_by(tau0, Time) %>%
  filter(Upper == min(Upper)) 



lowest_costs2$Costs = lowest_costs2$Lower
lowest_costs3$Costs = lowest_costs3$Upper

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
  #geom_line(data = lowest_costs2, aes(group = 1), linewidth = 0.5, alpha = 0.1) +
  #geom_line(data = lowest_costs3, aes(group = 1), linewidth = 0.5, alpha = 0.1) +
  facet_wrap(~ tau0, nrow = 3, ncol = 3, scales = "free_y", 
             labeller = labeller(tau0 = function(value) paste0("tau0 = ", value))) +
  labs(title = "Progress of economic cost per individual for the COVID-19 pandemic",
       x = "Time in days",
       y = "Economic cost per individual") +
  theme_bw() +
  theme(legend.position = "right",
        legend.key.size = unit(3, "lines")) +
  scale_color_manual(values = algorithm_colors)



