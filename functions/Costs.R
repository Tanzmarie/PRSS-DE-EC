calculateEconomicCosts <- function(cv, cm, cp, cl, tau, tau0, h, omega, n, mu, k, co) {
  # Deterministic costs
  DC <- ifelse(tau0 < tau, cv + tau0 * (cm + cp + cl), cv + tau * (cm + cp + cl))
  
  # Stochastic costs
  CS <- h * omega * (n - k) * mu
  
  # Outsource cost
  CO <- ifelse(tau0 < tau, (tau - tau0) * co, 0)
  
  # Total costs
  TotalCosts <- DC + CS + CO
  
  # Create a data frame
  Costs <- data.frame(
    Algorithm = c("Individual", "Dorfman", "Double Pooling", "Triple Pooling", "Three-stage", "Four-stage"),
    DC = DC,
    CS = CS,
    CO = CO,
    Costs = TotalCosts
  )
  
  return(Costs)
}
