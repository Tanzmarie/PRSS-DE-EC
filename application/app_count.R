library(readr)
library(readxl)
library(tidyverse)

source("functions/tests.R")
source("functions/costs.R")

# Load data and perform data manipulation

cov_dat = read_csv("application/data/COVID-19-Faelle_7-Tage-Inzidenz_Landkreise.csv")
krs_dat = read_excel("application/data/Kreis_2021-07-31.xlsx", skip = 7)

krs_dat = krs_dat[,c(2,3)]
names(krs_dat) = c("KrS","Kreis")

landkreis_loc = unique(cov_dat$Landkreis_id)

# Perform left join
cov_dat = left_join(cov_dat, krs_dat, by = c("Landkreis_id" = "KrS"))

# Locations for which you want to calculate tests

location = unique(cov_dat$Kreis)
set.seed(444)
locations = location[sample(length(location), size = 5, replace = FALSE)]

num_cores = detectCores() - 1
cl = makeCluster(num_cores)
registerDoParallel(cl)
plan(multisession, workers = detectCores() - 1)

# Initialize a list to store the results for each location
tests_list = list()



# Loop over locations
for (i in locations) {
  dt = cov_dat[which(cov_dat$Kreis == i),]
  loc = i
  
  dt$prevalence = ((dt$`Inzidenz_7-Tage`/7) * 14)/100000
  
  
  # Calculate the number of tests
  tests = future_map(dt$prevalence, calculate_tests, n = 1000, sims = 10)
  

  
  
  # Store the results in the list
  tests_list[[paste0("Location_", i)]] = list(tests = tests)
}

# Calculating economic costs
res = c(80.64042,87.35672,95.58348)

# Costs
n = 1000
cf = 1000
cv = 150
tau0 = 750
cl = 150
mu = res[2]
h  = 0.5



# Define a list to store the results for each location
economic_costs_list = list()
l_economic_costs_list = list()
u_economic_costs_list = list()



# Iterate over locations
for (loc_index in locations) {
  # Extract tests and k for the current location
  tests = tests_list[[paste0("Location_", loc_index)]][["tests"]]

  # Define a list to store the results for each time point
  result_costs_list = list()
  result_costs_list2 = list()
  result_costs_list3 = list()
  
  
  # Iterate over indices
  for (i in seq_along(tests)) {
    # Extract tau and omega for the current time point
    current_tau = tests[[i]]$Tests
    l_current_tau = tests[[i]]$Lower
    u_current_tau = tests[[i]]$Upper
    current_omega = tests[[i]]$Duration
    
    
    # Call calculateEconomicCosts for the current time point and fixed h value
    current_costs = calculateEconomicCosts(cf, cv, cl, current_tau, tau0, h, current_omega, n, mu)
    l_current_costs = calculateEconomicCosts(cf, cv, cl, l_current_tau, tau0, h, current_omega, n, mu)
    u_current_costs = calculateEconomicCosts(cf, cv, cl, u_current_tau, tau0, h, current_omega, n, mu)
    
    
    # Store the result in the list
    result_costs_list[[i]] = current_costs
    result_costs_list2[[i]] = l_current_costs
    result_costs_list3[[i]] = u_current_costs
  }
  
  # Store the results for this h value
  economic_costs_list[[as.character(loc_index)]] = result_costs_list
  l_economic_costs_list[[as.character(loc_index)]] = result_costs_list2
  u_economic_costs_list[[as.character(loc_index)]] = result_costs_list3
  
}

# Combine the results for different locations
result_costs = do.call(rbind, lapply(names(economic_costs_list), function(loc_index) {
    data.frame(
      Time = rep(seq_along(economic_costs_list[[loc_index]]), each = nrow(economic_costs_list[[loc_index]][[1]])),
      Algorithm = rep(economic_costs_list[[loc_index]][[1]][, "Algorithm"], times = length(economic_costs_list[[loc_index]])),
      DC = unlist(lapply(economic_costs_list[[loc_index]], function(result) result[, "DC"])),
      CS = unlist(lapply(economic_costs_list[[loc_index]], function(result) result[, "CS"])),
      Costs = unlist(lapply(economic_costs_list[[loc_index]], function(result) result[, "Costs"])),
      Location = rep(loc_index, each = nrow(economic_costs_list[[loc_index]][[1]]))
    )
}))

result_costs2 = do.call(rbind, lapply(names(l_economic_costs_list), function(loc_index) {
  data.frame(
    Time = rep(seq_along(l_economic_costs_list[[loc_index]]), each = nrow(l_economic_costs_list[[loc_index]][[1]])),
    Algorithm = rep(l_economic_costs_list[[loc_index]][[1]][, "Algorithm"], times = length(l_economic_costs_list[[loc_index]])),
    DC = unlist(lapply(l_economic_costs_list[[loc_index]], function(result) result[, "DC"])),
    CS = unlist(lapply(l_economic_costs_list[[loc_index]], function(result) result[, "CS"])),
    Costs = unlist(lapply(l_economic_costs_list[[loc_index]], function(result) result[, "Costs"])),
    Location = rep(loc_index, each = nrow(l_economic_costs_list[[loc_index]][[1]]))
  )
}))

result_costs3 = do.call(rbind, lapply(names(u_economic_costs_list), function(loc_index) {
  data.frame(
    Time = rep(seq_along(u_economic_costs_list[[loc_index]]), each = nrow(u_economic_costs_list[[loc_index]][[1]])),
    Algorithm = rep(u_economic_costs_list[[loc_index]][[1]][, "Algorithm"], times = length(u_economic_costs_list[[loc_index]])),
    DC = unlist(lapply(u_economic_costs_list[[loc_index]], function(result) result[, "DC"])),
    CS = unlist(lapply(u_economic_costs_list[[loc_index]], function(result) result[, "CS"])),
    Costs = unlist(lapply(u_economic_costs_list[[loc_index]], function(result) result[, "Costs"])),
    Location = rep(loc_index, each = nrow(u_economic_costs_list[[loc_index]][[1]]))
  )
}))

result_costs$Lower = result_costs2$Costs
result_costs$Upper = result_costs3$Costs

# Filter the cov_dat to keep only the lowest cost line for each facet
lowest_costs = result_costs %>%
  group_by(Location, Time) %>%
  filter(Costs == min(Costs))

lowest_costs2 = result_costs %>%
  group_by(Location, Time) %>%
  filter(Lower == min(Lower))

lowest_costs3 = result_costs %>%
  group_by(Location, Time) %>%
  filter(Upper == min(Upper)) 

lowest_costs2$Costs = lowest_costs2$Lower
lowest_costs3$Costs = lowest_costs3$Upper

# Plotting

coordinates = as.vector(locations)
coordinates = setNames(coordinates, locations)

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
  facet_wrap(~ Location, nrow = 5, ncol = 1, scales = "free_y", 
             labeller = labeller(Location = function(value) {
               return(coordinates[value])
             })) +
  labs(title = "Progress of economic cost per individual over the COVID-19 pandemic horizon in German districts",
       x = "Time in days",
       y = "Economic cost per individual") +
  theme_bw() +
  theme(legend.position = "right",
        legend.key.size = unit(3, "lines")) +
scale_color_manual(values = algorithm_colors)





