# Estimating incomes with rstan
library(rstan)

options(mc.cores = parallel::detectCores())


set.seed(100)  # Set seed for reproducibility

generate_group_params <- function(num_groups, mean_range, sd_range) {
  group_params <- list()
  for (i in 1:num_groups) {
    meanlog <- runif(1, min = mean_range[1], max = mean_range[2])
    sdlog <- runif(1, min = sd_range[1], max = sd_range[2])
    group_params[[paste0("group", i)]] <- list(meanlog = meanlog, sdlog = sdlog)
  }
  return(group_params)
}

# Generate group parameters
num_groups <- 3
mean_range <- c(2.5, 4)  
sd_range <- c(0.1, 1)    
group_params <- generate_group_params(num_groups, mean_range, sd_range)

# Number of samples per group
n_per_group <- 500

# Generate data for each group
income_data <- c()

for (group in names(group_params)) {
  params <- group_params[[group]]
  group_data <- rlnorm(n_per_group, meanlog = params$meanlog, sdlog = params$sdlog)
  income_data <- c(income_data, group_data)
}




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
