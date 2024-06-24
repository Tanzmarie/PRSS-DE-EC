# Loading Dependencies
library(rstan)
library(tidyverse)
library(readr)
library(maxLik)
library(boot)
library(rsample)


# Parallel Computing
options(mc.cores = parallel::detectCores())


# Import data sets
pgen = read_csv("C:/Users/mbalzer/Desktop/SOEP-CORE.v38.1_eu_CSV/CSV/soepdata/pgen.csv")
pequiv = read_csv("C:/Users/mbalzer/Desktop/SOEP-CORE.v38.1_eu_CSV/CSV/soepdata/pequiv.csv")

pgen = read_csv("D:/Universität/PhD/Project 1/Data/cs-transfer/SOEP-CORE.v38.1_eu_CSV/CSV/soepdata/pgen.csv")
pequiv = read_csv("D:/Universität/PhD/Project 1/Data/cs-transfer/SOEP-CORE.v38.1_eu_CSV/CSV/soepdata/pequiv.csv")

# Data Preparation
inc = pgen %>%
  filter(pglabgro > 0 & pgtatzeit > 0 & syear %in% seq(2019,2021,1)) %>%
  group_by(pid) %>%
  filter(syear == max(syear)) %>%
  ungroup()


loc = pequiv %>%
  filter(l11101 > 0 & syear %in% seq(2019,2021,1) & pid %in% inc$pid) %>%
  group_by(pid) %>%
  filter(syear == max(syear)) %>%
  ungroup()

# Join datasets
dt = inner_join(inc, loc[, c("pid", "l11101")], by = "pid")

# Calculate daily income
dt = dt %>%
  mutate(dailyinc = (pglabgro / (pgtatzeit / 5)) / 4.345)

hb = dt %>%
  filter(l11101 == 2)


hb = list(N = nrow(hb), J = length(unique(hb$l11101)), loc = hb$l11101, CS = hb$dailyinc)

# Calculate MLE estimate
llf = function(params) {
  mu = params[1]
  sd = params[2]
  llValue = dlnorm(hb$CS, mean = mu, sd = sd, log=TRUE)
  sum(llValue)
}

summary(maxLik(llf, start = c(mu=0.1, sd=0.1)))

# Bagging

# Parameters
data <- hb$CS  # Example dataset
n_iterations <- 100  # Number of bootstrap samples
sample_size <- length(data)  # Size of each bootstrap sample
aggregate_size <- 50  # Number of samples you want after aggregation

sample(unlist(lapply(1:100, function(i) {sample(data, replace = TRUE)})), size = 1000, replace = FALSE)


# Some tests with costs
n = 1000
opts = 3
p_groups = 50
num_tests = 425
cv = 150
cl = 300
tau0 = 750 
h = 0.5

cost = function(n,opts,p_groups, tau0, cv, cl, num_tests, h, data) {
F1 = sample(data, n, replace = TRUE)
F2 = sample(F1, p_groups * opts)

DC = ifelse(tau0 < num_tests, tau0 * cv, num_tests * cv)
# Stochastic costs
CS = (1-h) * (sum(F1) + sum(F2))

# Outsource cost
CO = ifelse(tau0 < num_tests, (num_tests - tau0) * cl, 0)

# Total costs
(DC + CS + CO) / n
}

cost2 = function(n,opts,p_groups, tau0, cv, cl, num_tests, h, data) {
  F1 = sample(replicate(sample(data, replace = TRUE), n = 100), size = n)
  F2 = sample(F1, p_groups * opts)
  
  DC = ifelse(tau0 < num_tests, tau0 * cv, num_tests * cv)
  # Stochastic costs
  CS = (1-h) * (sum(F1) + sum(F2))
  
  # Outsource cost
  CO = ifelse(tau0 < num_tests, (num_tests - tau0) * cl, 0)
  
  # Total costs
  (DC + CS + CO) / n
}

mean(replicate(cost(data = hb$CS, n,opts,p_groups, tau0, cv, cl, num_tests, h), n = 50))
mean(replicate(cost2(data = hb$CS, n,opts,p_groups, tau0, cv, cl, num_tests, h), n = 50))


# Define the Stan model
stan_model = "
data {
  int N; 
  int J;
  int<lower = 1, upper = J> loc[N];
  vector[N] CS;              
}

parameters {
  vector[J] mu;           
  vector<lower=0>[J] sigma;    
  real w;           
  real<lower=0> k; 
  real a;
  real<lower=0> b;
}

model {
  // Stage 1: Likelihood
  for(n in 1:N) {
    CS[n] ~ lognormal(mu[loc[n]], sigma[loc[n]]);
  }
  
  // Stage 2: Prior for mu and sigma
  mu ~ normal(w,k);
  sigma ~ cauchy(a,b);

  // Stage 3: Hyperpriors
   w ~ normal(0,5);
   k ~ cauchy(0,25);
   a ~ normal(0,5);
   b ~ cauchy(0,25);
  
}
"

# Compile the model
compiled_model = stan_model(model_code = stan_model)

# Run the MCMC sampler
fit = sampling(compiled_model, data = hb, chains = 4, iter = 2000, warmup = 1000, thin = 1, control = list(adapt_delta = 0.8))
fit
get_posterior_mean(fit)
test = get_sampler_params(fit)

# Print summary of the results
x11()
print(fit)
plot(fit)
traceplot(fit)
stan_trace(fit)
stan_hist(fit)
stan_dens(fit)
stan_diag(fit)
