library(readr)
library(readxl)
library(tidyverse)

source("functions/Tests.R")
source("functions/Costs.R")

# Load data and perform data manipulation

cov_dat <- read_csv("application/COVID-19-Faelle_7-Tage-Inzidenz_Landkreise.csv")
krs_dat <- read_excel("application/Kreis_2021-07-31.xlsx", skip = 7)

krs_dat = krs_dat[,c(2,3)]
names(krs_dat) = c("KrS","Kreis")

landkreis_loc = unique(cov_dat$Landkreis_id)

# Perform left join
cov_dat <- left_join(cov_dat, krs_dat, by = c("Landkreis_id" = "KrS"))

# location: "09375" == Regensburg
# Locations for which you want to calculate tests

location = unique(cov_dat$Kreis)
locations <- location[sample(length(location), size = 10, replace = FALSE)]

# Initialize a list to store the results for each location
tests_list <- list()



# Loop over locations
for (i in locations) {
  dt = cov_dat[which(cov_dat$Kreis == i),]
  loc = i
  
  dt$prevalence = ((dt$`Inzidenz_7-Tage`/7) * 14)/100000
  dt$prevalence2 = dt$`Faelle_7-Tage`/dt$Bevoelkerung
  
  
  # Calculate the number of tests
  tests <- lapply(X = dt$prevalence, calculate_tests, n = 1000)
  
  # Calculate k for the current location
  k <- unlist(round(1000 * dt$prevalence)) 
  
  
  # Store the results in the list
  tests_list[[paste0("Location_", i)]] <- list(tests = tests, k = k)
}

# Calculating economic costs

# Extract mu values from Simulation of Incomes
res = exp(c(3.34,3.49,3.64))

# Costs
n = 1000
cv = 1000
cm = 25
cp = 50
cl = 25
tau0 = 850
h = 0.5
mu = res[2]
co = 150



# Define a list to store the results for each location
economic_costs_list <- list()




# Iterate over locations
for (loc_index in locations) {
  # Extract tests and k for the current location
  tests <- tests_list[[paste0("Location_", loc_index)]][["tests"]]
  k <- tests_list[[paste0("Location_", loc_index)]][["k"]]
  
  # Define a list to store the results for each time point
  result_costs_list <- list()
  
  # Iterate over indices
  for (i in seq_along(tests)) {
    # Extract tau and omega for the current time point
    current_tau <- tests[[i]]$Tests
    current_omega <- tests[[i]]$Waiting.Times
    
    # Calculate k based on your cov_dat (adjust as needed)
    k_cur <- k[i]
    
    # Call calculateEconomicCosts for the current time point and fixed h value
    current_costs <- calculateEconomicCosts(cv, cm, cp, cl, current_tau, tau0, h, current_omega, n, mu, k_cur, co)
    
    # Store the result in the list
    result_costs_list[[i]] <- current_costs
  }
  
  # Store the results for this h value
  economic_costs_list[[as.character(loc_index)]] <- result_costs_list
  
}

# Combine the results for different locations
result_costs <- do.call(rbind, lapply(names(economic_costs_list), function(loc_index) {
    data.frame(
      Time = rep(seq_along(economic_costs_list[[loc_index]]), each = nrow(economic_costs_list[[loc_index]][[1]])),
      Algorithm = rep(economic_costs_list[[loc_index]][[1]][, "Algorithm"], times = length(economic_costs_list[[loc_index]])),
      DC = unlist(lapply(economic_costs_list[[loc_index]], function(result) result[, "DC"])),
      CS = unlist(lapply(economic_costs_list[[loc_index]], function(result) result[, "CS"])),
      Costs = unlist(lapply(economic_costs_list[[loc_index]], function(result) result[, "Costs"])),
      Location = rep(loc_index, each = nrow(economic_costs_list[[loc_index]][[1]]))
    )
}))

# Filter the cov_dat to keep only the lowest cost line for each facet
lowest_costs <- result_costs %>%
  group_by(Location, Time) %>%
  filter(Costs == min(Costs, na.rm = TRUE)) %>%
  ungroup()


# Plotting

coordinates <- as.vector(locations)
coordinates <- setNames(coordinates, locations)

x11()
ggplot(result_costs, aes(x = Time, y = Costs, color = Algorithm)) +
  geom_point(aes(group = Algorithm), alpha = 0.1, shape = ".") +  
  geom_line(data = lowest_costs, aes(group = 1), size = 1) +
  facet_wrap(~ Location, nrow = 5, ncol = 4, scales = "free_y", 
             labeller = labeller(Location = function(value) {
               return(coordinates[value])
             })) +
  labs(title = "Evolution of Economic Costs over Time",
       x = "Time",
       y = "Costs") +
  ylim(0, 125000) +
  theme_bw() +
  theme(legend.position = "right",
        legend.key.size = unit(7, "lines"))




