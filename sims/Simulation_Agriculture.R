# Dependencies
library(rstan)

options(mc.cores = parallel::detectCores())

source("functions/Tests.R")
source("functions/Costs.R")

# Load data for the epidemics


# Tests
result_dataframe <- calculate_tests(n = 1000, rho = 0.05)
print(result_dataframe)


# Using MCMC to capture Uncertainty of Economic Costs
# Simulate data from LN(40, 4)
set.seed(123)  # Set seed for reproducibility
simulated_data <- rlnorm(1000, meanlog = log(15), sdlog = sqrt(4))

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
  CS ~ lognormal(mu, sqrt(4));

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
  N = length(simulated_data),
  CS = simulated_data
)

# Run the MCMC sampler
fit <- sampling(stan_model, data = data_list, chains = 4, iter = 10000)

# Print summary of the results
print(fit)

res = c(mean(extract(fit)$mu),mean(extract(fit)$mu) - 1.96 * sd(extract(fit)$mu),mean(extract(fit)$mu) + 1.96 * sd(extract(fit)$mu))
res = exp(res)


# Costs
result_dataframe <- calculateEconomicCosts(cv, cm, cp, cl, tau, tau0, h, omega, n, mu, k, co)
print(result_dataframe)

