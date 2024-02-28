calculate_tests <- function(n, rho) {
  
  # Dorfman's 2-Stage Pooling algorithm
  dorfman <- function(n, rho) {
    if (rho < 0.01) {
      result = 2 * sqrt(rho) * n
    } else {
      opt = function(n, rho, s) {
        res = n * ((1/s) + 1 - (1 - rho)^s)
      }
      result = optimize(f = opt, interval = c(1, 200), n = n, rho = rho)$objective
    }
    return(result)
  }
  
  # Grid algorithm
  grid <- function(n, rho) {
    opt = function(s, n, rho) {
      res =  n * ((2/s) + rho + (1 - rho) * (1 - (1 - rho)^(s - 1))^2)
    }
    result = optimize(f = opt, interval = c(1, 200), n = n, rho = rho)$objective
    return(result)
  }
  
  
  # R-Pooling algorithm
  rpooling = function(n,rho) {
    if (rho < 0.01) {
      result = (1/log(2))*n*rho*log(1/rho)
    } else {
      opt = function(s, r, n, rho) {
        res = n * ((r/s) + rho + (1 - rho) * (1 - (1 - rho)^(s - 1))^r)
      }
      result = optim(par = c(s = 1, r = 1), fn = function(params) opt(params["s"], params["r"], n, rho), method = "L-BFGS-B", lower = c(s = 1, r = 1), upper = c(s = 100, r = 10))$value
    }
    return(result)
  }
  
  
  # Mutesa et al.'s Hypercube algorithm
  hypercube = function(n, rho) {
    if (rho < 0.01) {
    s = 0.350 / rho
    result = s * exp(1) * rho * log((0.734 / rho))
    result = result * (n/s)
    } else {
      result = NA
    }
    return(result)
  }
  
  # Patel's Multistage algorithm
  multi = function(n, rho, stage) {
    result = (stage + 1) * n * (rho)^(stage / (stage + 1)) + 1
    return(result)
  }
  
  # Generalized Binary Splitting
  binary = function(n, rho) {
    opt = function(s, n, rho) {
      res = n * ((1 + (1 - (1 - rho)^(2^s)) * s) / ((1 / rho) * (1 + 2^s * (1 - rho)^(2^s + 1) - (2^s + 1) * (1 - rho)^(2^s)) + 2^s * (1 - rho)^(2^s)))
    }
    result = optimize(f = opt, interval = c(1, 100), n = n, rho = rho)$objective
    return(result)
  }
  
  
  # Calculate the number waiting times
  df <- data.frame(
    Algorithm = c("Individual", "Dorfman", "Grid", "Hypercube", "3-Stage", "4-Stage", "Binary Splitting"),
    'Waiting Times' = c(0, 1, 1, 1, 2, 3, log2(1/rho))
  )
  
  # Calculate the number of expected number of tests
  rho = rho + .Machine$double.xmin
  
  tests <- floor(c(n, dorfman(n,rho), grid(n,rho), hypercube(n, rho), multi(n, rho, 2), multi(n, rho, 3), binary(n,rho)))
  
  df$Tests <- tests
  df$k = round(rho * n)
  
  
  if (unique(df$k) == 0) {
    df$Tests[-1] = 1
    df$Waiting.Times[-1] = 1
    
  } 
  
  
  int10 = df[3] + 0.1 * df[3]
  int10[1,] = 1000
  int10 = floor(int10)
  df["10% Positive"] = int10
  
  intn10 = df[3] - 0.1 * df[3]
  intn10[1,] = 1000
  intn10 = floor(intn10)
  df["10% Negative"] = intn10
  
  
  return(df)
}


