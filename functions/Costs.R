calculateEconomicCosts = function(cf, cv, cl, tau, tau0, h, omega, n, mu) {
  # Deterministic costs
  DC = ifelse(tau0 < tau, cf + tau0 * cv, cf + tau * cv)
  
  # Stochastic costs
  CS = (1-h) * omega * n  * mu
  
  # Outsource cost
  CO = ifelse(tau0 < tau, (tau - tau0) * cl, 0)
  
  # Total costs
  TotalCosts = DC + CS + CO
  
  # Create a data frame
  Costs = data.frame(
    Algorithm = c("Individual",
                  "Dorfman",
                  #"Double-Pooling",
                  #"R-Pooling",
                  "3-Stage",
                  "4-Stage"),
    DC = DC/n,
    CS = CS/n,
    CO = CO/n,
    Costs = TotalCosts/n
  )
  
  return(Costs)
}
