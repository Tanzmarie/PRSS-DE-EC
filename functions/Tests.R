calculate_tests <- function(n, rho) {
  # Define the objective function for expected number of tests
  dorfman <- function(s, n, rho) {
    result <- n * ((1/s) + 1 - (1 - rho)^s)
    return(result)
  }
  
  rpooling <- function(s, r, n, rho) {
    result <- n * ((r/s) + rho + (1 - rho) * (1 - (1 - rho)^(s - 1))^r)
    return(result)
  }
  
  hypercube <- function(n, rho) {
    s <- 0.350 / rho
    result <- s * exp(1) * rho * log((0.734 / rho))
    result <- result * (n/s)
    return(result)
  }
  
  multi <- function(n, rho, stage) {
    result <- (stage + 1) * n * (rho)^(n / (n + 1)) + 1
    return(result)
  }
  
  binary <- function(s, n, rho) {
    result <- n * ((1 + (1 - (1 - rho)^(2^s)) * s) / ((1 / rho) * (1 + 2^s * (1 - rho)^(2^s + 1) - (2^s + 1) * (1 - rho)^(2^s)) + 2^s * (1 - rho)^(2^s)))
    return(result)
  }
  
  # Calculate the expected number of floors, i.e., waiting times
  wtimes <- data.frame(
    Algorithm = c("Individual", "Dorfman", "RPooling", "Hypercube", "3-Stage", "4-Stage", "Binary Splitting A", "Binary Splitting B"),
    'Waiting Times' = c(0, 1, 1, 1, 2, 3, floor(log2(1/rho)) - 1, floor(log2(1/rho)) - 1)
  )
  
  optimal_tdorf <- optimize(f = dorfman, interval = c(1, 15), n = n, rho = rho)$objective
  optimal_trpool <- optim(par = c(s = 1, r = 1), fn = function(params) rpooling(params["s"], params["r"], n, rho), method = "L-BFGS-B", lower = c(s = 1, r = 1), upper = c(s = 10, r = 15))$value
  optimal_tbinary <- optimize(f = binary, interval = c(1, 100), n = n, rho = rho)$objective
  
  tests <- floor(c(n, optimal_tdorf, optimal_trpool, hypercube(n, rho), multi(n, rho, 2), multi(n, rho, 3), binary(1, n, rho), optimal_tbinary))
  
  wtimes$Tests <- tests
  
  
  
  int10 = wtimes[3] + 0.1 * wtimes[3]
  int10[1,] = 1000
  int10 = floor(int10)
  wtimes["10% Positive"] = int10
  
  intn10 = wtimes[3] - 0.1 * wtimes[3]
  intn10[1,] = 1000
  intn10 = floor(intn10)
  wtimes["10% Negative"] = intn10
  
  return(wtimes)
}

# Example usage:
result_dataframe <- calculate_tests(n = 1000, rho = 0.05)
print(result_dataframe)












