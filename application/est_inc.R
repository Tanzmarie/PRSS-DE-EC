# Loading Dependencies
library(rstan)
library(tidyverse)
library(readr)


# Parallel Computing
options(mc.cores = parallel::detectCores())

# Import data sets
pgen = read_csv("C:/Users/mbalzer/Desktop/SOEP-CORE.v38.1_eu_CSV/CSV/soepdata/pgen.csv")
pequiv = read_csv("C:/Users/mbalzer/Desktop/SOEP-CORE.v38.1_eu_CSV/CSV/soepdata/pequiv.csv")

pgen = read_csv("D:/Universität/PhD/Project 1/Data/cs-transfer/SOEP-CORE.v38.1_eu_CSV/CSV/soepdata/pgen.csv")
pequiv = read_csv("D:/Universität/PhD/Project 1/Data/cs-transfer/SOEP-CORE.v38.1_eu_CSV/CSV/soepdata/pequiv.csv")

# Data Preparation
inc <- pgen %>%
  filter(pglabgro > 0 & pgtatzeit > 0 & syear %in% c(2019, 2020, 2021)) %>%
  group_by(pid) %>%
  filter(syear == max(syear)) %>%
  ungroup()

loc <- pequiv %>%
  filter(l11101 > 0 & syear %in% c(2019, 2020, 2021) & pid %in% inc$pid) %>%
  group_by(pid) %>%
  filter(syear == max(syear)) %>%
  ungroup()

# Join datasets
dt <- inner_join(inc, loc[, c("pid", "l11101")], by = "pid")

# Calculate daily income
dt <- dt %>%
  mutate(dailyinc = (pglabgro / (pgtatzeit / 5)) / 4.345)

hb <- dt %>%
  filter(l11101 == 2)

hb = hb %>%
    filter(dailyinc < 1000)

hb <- list(N = nrow(hb), CS = hb$dailyinc)

rm(dt,inc,loc,pequiv,pgen)
  
# Define the Stan model
stan_model = "
data {
  int<lower=1> N;             // Number of observations
  vector[N] CS;               // Vector of incomes
}

parameters {
  real<lower=0> mu;           // Mean parameter of log-normal distribution
  real<lower=0> sigma;        // Standard deviation parameter of log-normal distribution
  real<lower=0> w;            // Mean parameter of mu distribution
  real<lower=0> p;           // Variance parameter of mu distribution (squared)
}

model {
  // Stage 1: Likelihood
  CS ~ lognormal(mu, sigma);  // Log-normal likelihood
  
  // Stage 2: Prior for mu
  mu ~ lognormal(w, p); // Log-normal prior for mu
  
  // Stage 2: Prior for sigma
  sigma ~ inv_gamma(1,1); // Inverse gamma prior for sigma
  
  // Stage 3: Prior for w
  w ~ exponential(1);          // Exponential prior for w
  
  // Stage 3: Prior for p2
  p ~ inv_gamma(1,1);     // Inverse gamma prior for p2
}
"

# Compile the model
compiled_model = stan_model(model_code = stan_model)

# Run the MCMC sampler seed = 2000, if excluded seed = 3049
fit = sampling(compiled_model, data = hb, seed = 2000, chains = 4, iter = 2000, warmup = 1000, thin = 1, control = list(adapt_delta = 0.9))

# Print summary of the results
fit
