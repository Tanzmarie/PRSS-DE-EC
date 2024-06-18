# Loading Dependencies
library(rstan)
library(tidyverse)
library(readr)
library(maxLik)
library(boot)


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

hb = hb %>% filter(pgnace2 > 0)

hb$pgnace2[which(hb$pgnace2 == -1)] = 100
hb$pgnace2[which(hb$pgnace2 == -2)] = 101

# Plot density and histogram

ggplot(hb, aes(x=dailyinc)) + 
  geom_histogram(aes(y=after_stat(density)), fill="white", color="black", bins=60) +  
  geom_density(alpha=0.2, fill="#FF6666") +  
  labs(title = "Histogram and kernel density of incomes in Hamburg",
       x = "Daily incomes",
       y = "Density") +
  theme_bw() 


hb = list(N = nrow(hb), J = length(unique(hb$l11101)), loc = hb$l11101, CS = hb$dailyinc)

# Calculate MLE estimate
llf = function(params) {
  mu = params[1]
  sd = params[2]
  llValue = dlnorm(hb$CS, mean = mu, sd = sd, log=TRUE)
  sum(llValue)
}

summary(maxLik(llf, start = c(mu=0.1, sd=0.1)))

# Bootstraping

mean(sample(hb$CS,10000, replace = TRUE))
mean(rlnorm(10000,4.45,0.97))

sample(hb$CS, 100, replace = TRUE)

mean_func <- function(data, indices) {
  sample <- data[indices]
  return(mean(sample))
}

boot(data = hb$CS, statistic = mean_func, R = 100)

# Define the Stan model
stan_model = "
data {
  int N; 
  int J;
  int<lower = 1, upper = J> loc[N];
  vector[N] CS;              
}

parameters {
  real mu;           
  real<lower=0> sigma;    
  vector[J] w;           
  vector<lower=0>[J] k; 
  vector[J] a;
  vector<lower=0>[J] b;
}

model {
  // Stage 1: Likelihood
    CS ~ lognormal(mu, sigma);
  
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
