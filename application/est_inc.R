# Loading Dependencies
library(rstan)
library(tidyverse)
library(readr)


# Parallel Computing
options(mc.cores = parallel::detectCores())
parallel:::setDefaultClusterOptions(setup_strategy = "sequential")


# Import data sets
pgen = read_csv("C:/Users/mbalzer/Desktop/SOEP-CORE.v38.1_eu_CSV/CSV/soepdata/pgen.csv")
pequiv = read_csv("C:/Users/mbalzer/Desktop/SOEP-CORE.v38.1_eu_CSV/CSV/soepdata/pequiv.csv")

pgen = read_csv("D:/Universität/PhD/Project 1/Data/cs-transfer/SOEP-CORE.v38.1_eu_CSV/CSV/soepdata/pgen.csv")
pequiv = read_csv("D:/Universität/PhD/Project 1/Data/cs-transfer/SOEP-CORE.v38.1_eu_CSV/CSV/soepdata/pequiv.csv")

# Data Preparation
inc <- pgen %>%
  filter(pglabgro > 0 & pgtatzeit > 0 & syear %in% seq(2019,2021,1)) %>%
  group_by(pid) %>%
  filter(syear == max(syear)) %>%
  ungroup()


loc <- pequiv %>%
  filter(l11101 > 0 & syear %in% seq(2019,2021,1) & pid %in% inc$pid) %>%
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


hb <- list(N = nrow(hb), CS = hb$dailyinc)

rm(dt,inc,loc,pequiv,pgen)


# Define the Stan model
stan_model = "
data {
  int N;             
  vector[N] CS;              
}

parameters {
  real mu;           
  real<lower=0> sigma;        
  real w;
  real<lower=0> p;
  real a;
  real<lower=0> b; 
}

model {
  // Stage 1: Likelihood
  CS ~ lognormal(mu, sigma); 
  
  // Stage 2: Prior for mu
  mu ~ normal(w, p); 
  
  // Stage 2: Prior for sigma
   sigma ~ cauchy(a,b); 
  
  // Stage 3: Prior for w
   w ~ normal(0, 1);          
  
  // Stage 3: Prior for p2
  p ~ cauchy(0,5);   
  
  a ~ normal(0, 1);
  b ~ cauchy(0,5);
}
"

# Compile the model
compiled_model = stan_model(model_code = stan_model)

# Run the MCMC sampler seed = 2000, if excluded seed = 3049
fit = sampling(compiled_model, data = hb, chains = 4, iter = 4000, warmup = 1000, thin = 1, control = list(adapt_delta = 0.8))
fit
get_posterior_mean(fit)
test = get_sampler_params(fit)

exp(4.39 + (0.98/2))
exp(4.47 + (0.98/2))
exp(4.56 + (0.98/2))

# Print summary of the results
print(fit)
plot(fit)
traceplot(fit)
stan_trace(fit)
stan_hist(fit)
stan_dens(fit)
stan_diag(fit)
