# Policy Recommendation for Sample Pooling Strategies in Dynamic Epidemics under Uncertainty of Economic Costs
Evaluate the optimal policy recommendation for the best pooling strategies under the assumption of an dynamically evolving epidemic if economic costs are subject to uncertainty. Uncertainty in costs is viewed as the potential loss of revenue or an latent increase in costs due to quarantining healthy individuals.

# Example

```
# Import functions
source("functions/Tests.R")
source("functions/Costs.R")

# Calculate number of expected tests in pooling algorithms

tests  = calculate_tests(100,0.05)
tau = tests["Tests"] 
omega = tests["Waiting.Times"]

# Define values for parameters
cv <- 10
cm <- 20
cp <- 15
cl <- 5
tau0 <- 50
h <- 0.1
n <- 100
mu <- 0.05
k <- 100 * 0.05
co <- 8

# Call the function with the provided values to calculate economic costs
result <- calculateEconomicCosts(cv, cm, cp, cl, tau, tau0, h, omega, n, mu, k, co)

# Print the result
print(result)

```
