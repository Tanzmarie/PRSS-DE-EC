# Dependencies
library(rstan)
library(knitr)

options(mc.cores = parallel::detectCores())

source("functions/DynamPrev.R")
source("functions/Tests.R")
source("functions/Costs.R")

# Load data for the epidemics

load("prevalence.RData")

# Find the prevalence value in region u_i in time t_i
target_time_point <- 500
location_point <- c(0.1,0.8)

closest_location <- find_closest_location(simulated_data, location_point)

subset_data <- simulated_data[simulated_data$t == target_time_point & 
                                simulated_data$u1 == closest_location[1] &
                                simulated_data$u2 == closest_location[2],]

prevalence_at_target <- subset_data$transformed_prevalence

print(paste("Closest location to target location", paste(location_point, collapse = ", "), 
            "is", paste(closest_location, collapse = ", ")))
print(paste("Prevalence at closest location and time point", target_time_point, ":", prevalence_at_target))

# Tests
result_tests <- calculate_tests(n = 1000, rho = prevalence_at_target)
print(result_tests)


# Using MCMC to capture Uncertainty of Economic Costs
# Simulate data from LN(40, 4)
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
res = c(mean(extract(fit)$mu),mean(extract(fit)$mu) - 1.96 * sd(extract(fit)$mu),mean(extract(fit)$mu) + 1.96 * sd(extract(fit)$mu))
res = exp(res)


# Costs
cv = 1000
cm = 25
cp = 50
cl = 25
tau0 = 850

tau = unlist(result_tests["Tests"])
omega = unlist(result_tests["Waiting.Times"])

h = 1
n = 1000
mu = res[1]
k = round(n * prevalence_at_target)
co = 150

result_costs <- calculateEconomicCosts(cv, cm, cp, cl, tau, tau0, h, omega, n, mu, k, co)
print(result_costs)

kable(result_costs, format = "latex")
