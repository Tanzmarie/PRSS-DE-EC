# Dependencies
library(rstan)
library(knitr)
library(tidyverse)

options(mc.cores = parallel::detectCores())

source("functions/DynamPrev.R")
source("functions/Tests.R")
source("functions/Costs.R")

# Load data for the epidemics

load("prevalence.RData")

# Find the prevalence value in region u_i in time t_i
location_point <- c(0.5,0.5)

closest_location <- find_closest_location(simulated_data, location_point)

subset_data = simulated_data[simulated_data$u1 == closest_location[1] &
               simulated_data$u2 == closest_location[2],]

# Calculate the number of tests
tests = lapply(X = unlist(subset_data[5]), calculate_tests, n = 1000)

# Assuming 'result' is the list of matrices you obtained
# Convert the list of matrices to a data frame

result_df <- do.call(rbind, lapply(seq_along(tests), function(i) {
  data.frame(
    Time = i,
    Algorithm = tests[[i]][1],
    Tests = tests[[i]][, "Tests"],
    Negative_Deviation = tests[[i]][, "10% Negative"],
    Positive_Deviation = tests[[i]][, "10% Positive"]
  )
}))

# Plotting
ggplot(result_df, aes(x = Time, y = Tests, color = Algorithm)) +
  geom_line(aes(group = Algorithm), size = 1) +
  geom_ribbon(aes(ymin = Negative_Deviation, ymax = Positive_Deviation, fill = Algorithm), alpha = 0.1) +
  labs(title = "Evolution of Tests Over Time",
       x = "Time",
       y = "Tests") +
  theme_minimal() +
  theme(legend.position = "right")

# Calulating economic costs

set.seed(100)  # Set seed for reproducibility
income_data <- rlnorm(500, meanlog = log(15), sdlog = sqrt(3))



# Define the Stan model
stan_code <- '
data {
  int<lower=0> N;  // Number of observations
  vector[N] CS;   // Observed values for Stage 1
}

parameters {
  real<lower=0> mu;      // Mean parameter for Stage 2
  real<lower=0> h;       // Hyperparameter for mu
  real<lower=0> p_sq;    // Squared scale parameter for mu
}

model {
  // Stage 1: Log-normal distribution for CS
  CS ~ lognormal(mu, sqrt(3));

  // Stage 2: Log-normal distribution for mu
  mu ~ lognormal(h, sqrt(p_sq));

  // Prior for hyperparameter h
  h ~ exponential(1);

  // Prior for squared scale parameter p^2
  p_sq ~ exponential(1);
}

generated quantities {
  // Posterior predictive checks or additional outputs can be added here
}
'

# Compile the model
stan_model <- stan_model(model_code = stan_code)

# Simulate or use real data
# Replace with your actual data
data_list <- list(
  N = length(income_data),
  CS = income_data
)

# Run the MCMC sampler
fit <- sampling(stan_model, data = data_list, chains = 4, iter = 10000)

# Print summary of the results
print(fit)

# extract mu values
res = exp(c(2.49,2.64, 2.79))

# Costs
n = 1000
cv = 1000
cm = 25
cp = 50
cl = 25
tau0 = 850

tau <- lapply(tests, function(mat) mat[, "Tests"])
omega <- lapply(tests, function(mat) mat[, "Waiting.Times"])
k = unlist(round(n * subset_data[5]))

h = 0.5
n = 1000
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

# Plotting with facet_grid

ggplot(result_costs, aes(x = Time, y = Costs, color = Algorithm)) +
  geom_line(aes(group = Algorithm), linewidth = 1, alpha = 0.5) +
  facet_wrap(~ h, nrow = 3, ncol = 4, scales = "free_y", labeller = labeller(h = function(value) paste0("h = ", value))) +
  labs(title = "Evolution of Deterministic Costs Over Time",
       x = "Time",
       y = "Deterministic Costs") +
  theme_light() +
  theme(legend.position = "right")

