library(readr)
library(tidyverse)
library(furrr)

source("functions/tests.R")
source("functions/costs.R")

data = read_csv("application/data/COVID-19-Faelle_7-Tage-Inzidenz_Landkreise.csv")
dt = data[which(data$Landkreis_id == "02000"),]

dt$prevalence = ((dt$`Inzidenz_7-Tage`/7) * 14)/100000


# Calculating the expected number of tests
num_cores = detectCores() - 1
cl = makeCluster(num_cores)
registerDoParallel(cl)
plan(multisession, workers = detectCores() - 1)

tests = future_map(dt$prevalence, calculate_tests, n = 1000, sims = 50)

stopCluster(cl)



# Calculating economic costs
res = c(exp(4.39 + (0.98/2)),exp(4.47 + (0.98/2)),exp(4.56 + (0.98/2)))


n = 1000
cf = 1000
tau0 = 750
cl = 300
mu = res[2]
h = 0.5

cv_values = c(0, 50, 150, 200, 300, 400)  


tau = lapply(tests, function(mat) mat[, "Tests"])
ltau = lapply(tests, function(mat) mat[, "Lower"])
utau = lapply(tests, function(mat) mat[, "Upper"])
omega = lapply(tests, function(mat) mat[, "Duration"])


# Define a list to store the results for each h
economic_costs_list = list()
l_economic_costs_list = list()
u_economic_costs_list = list()

# Iterate over h values
for (cv in cv_values) {
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
  economic_costs_list[[as.character(cv)]] = result_costs_list
  l_economic_costs_list[[as.character(cv)]] = result_costs_list2
  u_economic_costs_list[[as.character(cv)]] = result_costs_list3
}

# Combine the results for different h values
result_costs = do.call(rbind, lapply(names(economic_costs_list), function(cv) {
  data.frame(
    Time = rep(seq_along(economic_costs_list[[cv]]), each = nrow(economic_costs_list[[cv]][[1]])),
    Algorithm = rep(economic_costs_list[[cv]][[1]][, "Algorithm"], times = length(economic_costs_list[[cv]])),
    DC = unlist(lapply(economic_costs_list[[cv]], function(result) result[, "DC"])),
    CS = unlist(lapply(economic_costs_list[[cv]], function(result) result[, "CS"])),
    Costs = unlist(lapply(economic_costs_list[[cv]], function(result) result[, "Costs"])),
    cv = rep(as.numeric(cv), each = nrow(economic_costs_list[[cv]][[1]]))
  )
}))


result_costs2 = do.call(rbind, lapply(names(l_economic_costs_list), function(cv) {
  data.frame(
    Time = rep(seq_along(economic_costs_list[[cv]]), each = nrow(l_economic_costs_list[[cv]][[1]])),
    Algorithm = rep(l_economic_costs_list[[cv]][[1]][, "Algorithm"], times = length(l_economic_costs_list[[cv]])),
    DC = unlist(lapply(l_economic_costs_list[[cv]], function(result) result[, "DC"])),
    CS = unlist(lapply(l_economic_costs_list[[cv]], function(result) result[, "CS"])),
    Costs = unlist(lapply(l_economic_costs_list[[cv]], function(result) result[, "Costs"])),
    cv = rep(as.numeric(cv), each = nrow(l_economic_costs_list[[cv]][[1]]))
  )
}))

result_costs3 = do.call(rbind, lapply(names(u_economic_costs_list), function(cv) {
  data.frame(
    Time = rep(seq_along(economic_costs_list[[cv]]), each = nrow(u_economic_costs_list[[cv]][[1]])),
    Algorithm = rep(u_economic_costs_list[[cv]][[1]][, "Algorithm"], times = length(u_economic_costs_list[[cv]])),
    DC = unlist(lapply(u_economic_costs_list[[cv]], function(result) result[, "DC"])),
    CS = unlist(lapply(u_economic_costs_list[[cv]], function(result) result[, "CS"])),
    Costs = unlist(lapply(u_economic_costs_list[[cv]], function(result) result[, "Costs"])),
    cv = rep(as.numeric(cv), each = nrow(u_economic_costs_list[[cv]][[1]]))
  )
}))



# Filter the data to keep only the lowest cost line for each facet
lowest_costs = result_costs %>%
  group_by(cv, Time) %>%
  filter(Costs == min(Costs))

lowest_costs2 = result_costs2 %>%
  group_by(cv, Time) %>%
  filter(Costs == min(Costs)) 

lowest_costs3 = result_costs3 %>%
  group_by(cv, Time) %>%
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
  facet_wrap(~ cv, nrow = 3, ncol = 3, scales = "free_y", 
             labeller = labeller(cv = function(value) paste0("cv = ", value))) +
  labs(title = "Progress of economic cost per individual for the COVID-19 pandemic in Hamburg",
       x = "Time in days",
       y = "Economic cost per individual") +
  ylim(50,250) +
  xlim(0,1500) +
  theme_bw() +
  theme(legend.position = "right",
        legend.key.size = unit(3, "lines")) +
  scale_color_manual(values = algorithm_colors)


