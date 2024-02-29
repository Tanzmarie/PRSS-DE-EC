library(readr)
library(tidyverse)

source("functions/Tests.R")
source("functions/Costs.R")

COVID_19_Faelle_7_Tage_Inzidenz_Landkreise <- read_csv("application/COVID-19-Faelle_7-Tage-Inzidenz_Landkreise.csv")

data = COVID_19_Faelle_7_Tage_Inzidenz_Landkreise

# location: "09375" == Regensburg

location = unique(data$Landkreis_id)
dt = data[which(data$Landkreis_id == "09375"),]
loc = "09375"


dt$prevalence = ((dt$`Inzidenz_7-Tage`/7) * 14)/100000
dt$prevalence2 = dt$`Faelle_7-Tage`/dt$Bevoelkerung


# Calculating the expected number of tests

tests = lapply(X = dt$prevalence, calculate_tests, n = 1000)


# Calculating economic costs

# Extract mu values from Simulation of Incomes
res <- exp(c(3.34, 3.49, 3.64))

# Costs
n <- 1000
cv <- 1000
cp <- 50  
cm_values <- c(0, 10, 25, 50, 100, 200)  
cl <- 25
tau0 <- 850

tau <- lapply(tests, function(mat) mat[, "Tests"])
omega <- lapply(tests, function(mat) mat[, "Waiting.Times"])
k = unlist(round(n * dt$prevalence))

mu <- res[2]
co <- 150
h <- 0.5

# Define a list to store the results for each c_m
economic_costs_list <- list()

# Iterate over c_m values
for (cm in cm_values) {
  # Define a list to store the results for each time point
  result_costs_list <- list()
  
  # Iterate over the indices
  for (i in seq_along(tau)) {
    # Extract tau and omega for the current time point
    current_tau <- tau[[i]]
    current_omega <- omega[[i]]
    
    # Calculate k based on your data (adjust as needed)
    k_cur <- k[i]
    
    # Call calculateEconomicCosts for the current time point, fixed h value, and varied c_m
    current_costs <- calculateEconomicCosts(cv, cm, cp, cl, current_tau, tau0, h, current_omega, n, mu, k_cur, co)
    
    # Store the result in the list
    result_costs_list[[i]] <- current_costs
  }
  
  # Store the results for this c_m value
  economic_costs_list[[as.character(cm)]] <- result_costs_list
}

# Combine the results for different c_m values
result_costs <- do.call(rbind, lapply(names(economic_costs_list), function(cm) {
  data.frame(
    Time = rep(seq_along(economic_costs_list[[cm]]), each = nrow(economic_costs_list[[cm]][[1]])),
    Algorithm = rep(economic_costs_list[[cm]][[1]][, "Algorithm"], times = length(economic_costs_list[[cm]])),
    DC = unlist(lapply(economic_costs_list[[cm]], function(result) result[, "DC"])),
    CS = unlist(lapply(economic_costs_list[[cm]], function(result) result[, "CS"])),
    Costs = unlist(lapply(economic_costs_list[[cm]], function(result) result[, "Costs"])),
    h = rep(h, each = nrow(economic_costs_list[[cm]][[1]])),
    CM = rep(as.numeric(cm), each = nrow(economic_costs_list[[cm]][[1]]))
  )
}))

# Filter the data to keep only the lowest cost line for each facet
lowest_costs <- result_costs %>%
  group_by(CM, Time) %>%
  filter(Costs == min(Costs, na.rm = TRUE)) %>%
  ungroup()



# Plotting with facet_grid
x11()
ggplot(result_costs, aes(x = Time, y = Costs, color = Algorithm)) +
  geom_point(aes(group = Algorithm), alpha = 0.1, shape = ".") +   
  geom_line(data = lowest_costs, aes(group = 1), size = 1) +
  facet_wrap(~ CM, nrow = 3, ncol = 2, scales = "free_y", labeller = label_both) +
  labs(title = "Evolution of Economic Costs over Time",
       x = "Time",
       y = "Costs") +
  #ylim(0, 125000) +
  theme_bw() +
  theme(legend.position = "right",
        legend.key.size = unit(7, "lines"))

