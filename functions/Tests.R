require("optimx")

calculate_tests <- function(n, rho) {
  
  individual = function(n) {
    df = data.frame("Tests" = n, "Duration" = 1)
    row.names(df) = "Individual"
    
    return(df)
  }
  
  # Dorfman's 2-Stage Pooling algorithm
  dorfman <- function(n, rho) {
    if (rho < 0.01) {
      opts = 1/sqrt(rho)
      result = 2 * sqrt(rho) * n
    } else {
      opt = function(n, rho, s) {
        res = n * ((1/s) + 1 - (1 - rho)^s)
      }
      
      optimization = optimx(par = c(s = 1), fn = function(params) opt(n, rho, params["s"]), method = "nlminb")
      
      if(optimization$s > n | optimization$value > n | optimization$convcode == 1) {
        result = n
        opts = 0
      } else {
        result = optimization$value
        opts = optimization$s
      }
    }
    
    if(round(n * rho) == 0) {
      result = 1
    }
    
    if(result == n) {
      w1 = 24
      w2 = 0
    } else {
      w1 = 24
      w2 = 24
    }
    
    qt = (w1 + (1-(1-rho)^opts)*w2) / 24
    
    df = data.frame("Tests" = result, "Duration" = qt)
    row.names(df) = "Dorfman"
    
    return(df)
  }
  
  # Grid algorithm
  double <- function(n, rho) {
    if (rho < 0.01) {
      opts = 1/rho^(2/3)
      result = 3 * rho^(2/3) * n
    } else {
      opt = function(n, rho, s) {
        res =  n * ((2/s) + rho + (1 - rho) * (1 - (1 - rho)^(s - 1))^2)
      }
      
      optimization = optimx(par = c(s = 1), fn = function(params) opt(n, rho, params["s"]), method = "nlminb")
      
      if(optimization$s > n | optimization$value > n | optimization$convcode == 1) {
        result = n
        opts = 1
      } else {
        result = optimization$value
        opts = optimization$s
      }
    }
    
    if(round(n * rho) == 0) {
      result = 1
    }
    
    if(result == n) {
      w1 = 24
      w2 = 0
    } else {
      w1 = 24
      w2 = 24
    }
    
    qt = (w1 + (rho + (1-rho)*(1-(1-rho)^(opts-1))^2)*w2) / 24
    
    df = data.frame("Tests" = result, "Duration" = qt)
    row.names(df) = "Double Pooling"
    
    return(df)
  }
  
  # # R-Pooling algorithm
  # rpooling = function(n,rho) {
  #   w1 = 24
  #   w2 = 24
  #   
  #   if (rho <= 0.01) {
  #     opts = log(2)/rho
  #     optr = log2(1/rho)
  #     result = (1/log(2))*n*rho*log(1/rho)
  #     
  #     if(round(n * rho) == 0) {
  #       result = 1
  #     }
  #     
  #     qt = (w1 + (rho + (1-rho)*(1-(1-rho)^(opts-1))^optr)*w2) / 24
  #     
  #     df = data.frame("Tests" = result, "Duration" = qt)
  #     row.names(df) = "R-Pooling"
  #   } else {
  #     opt = function(s, r, n, rho) {
  #       res = n * ((r/s) + rho + (1 - rho) * (1 - (1 - rho)^(s - 1))^r)
  #     }
  #     
  #     optimization = optimx(par = c(s = 1, r = 1), fn = function(params) opt(params["s"], params["r"], n, rho), method = c("L-BFGS-B"), lower = c(1,1), upper = c(35,20))
  #     
  #     if (optimization$r == 1) {
  #       df = dorfman(n,rho)
  #     } else {
  #       opts = optimization$s
  #       optr = optimization$r
  #       result = optimization$value
  #       
  #       if(round(n * rho) == 0) {
  #         result = 1
  #       }
  #       
  #       
  #       qt = (w1 + (rho + (1-rho)*(1-(1-rho)^(opts-1))^optr)*w2) / 24
  #       
  #       df = data.frame("Tests" = result, "Duration" = qt)
  #       row.names(df) = "R-Pooling"
  #     }
  #   }
  #   
  #   return(df)
  # }
  
  triple <- function(n, rho) {
    if (rho < 0.01) {
      opts = rho^(-3/4)
      result = rho + 4*rho^(3/4)
    } else {
      opt = function(n, rho, s) {
        res =  n * ((3/s) + rho + (1 - rho) * (1 - (1 - rho)^(s - 1))^3)
      }
      
      optimization = optimx(par = c(s = 1), fn = function(params) opt(n, rho, params["s"]), method = "nlminb")
      
      if(optimization$s > n | optimization$value > n | optimization$convcode == 1) {
        result = n
        opts = 1
      } else {
        result = optimization$value
        opts = optimization$s
      }
    }
    
    if(round(n * rho) == 0) {
      result = 1
    }
    
    if(result == n) {
      w1 = 24
      w2 = 0
    } else {
      w1 = 24
      w2 = 24
    }
    
    qt = (w1 + (rho + (1-rho)*(1-(1-rho)^(opts-1))^3)*w2) / 24
    
    df = data.frame("Tests" = result, "Duration" = qt)
    row.names(df) = "Triple Pooling"
    
    return(df)
  }
  
  # Patel's 3-Stage algorithm
  patel3 = function(n, rho) {
    if (rho < 0.01) {
      result = 3*rho^(2/3)*n 
      opts1 = 1/rho^(2/3)
      opts2 = 1/rho^(1/3)
    } else {
      opt =  function(n, rho, s1, s2) {
        res = n*(1/s1 + 1/s2*(1 - (1-rho)^s1) + (1 - (1-rho)^s2))
      }
      
      optimization = optimx(par = c(s1 = 0.1, s2 = 0.1), fn = function(params) opt(n, rho, params["s1"], params["s2"] ), method = c("nlminb"))
      
      if(optimization$s1 > n | optimization$value > n | optimization$value == -Inf | optimization$convcode == 1) {
        result = n
        opts1 = 0
        opts2 = 0
      } else {
        result = optimization$value
        opts1 = optimization$s1
        opts2 = optimization$s2
      }
    }
    
    if(round(n * rho) == 0) {
      result = 1
    }
    
    if(result == n) {
      w1 = 24
      w2 = 0
      w3 = 0
    } else {
      w1 = 24
      w2 = 24
      w3 = 24
    }
    
    qt = (n*w1 + n*(1-(1-rho)^opts1)*w2 + n * (1-(1-rho)^opts1) * (1-(1-rho)^opts2) * w3) / (n*24) 
    
    df = data.frame("Tests" = result, "Duration" = qt)
    row.names(df) = "Three-stage"
    
    return(df)
  }
  
  # Patel's 4-Stage algorithm
  
  patel4 = function(n, rho) {
    if(rho < 0.01) {
      result = 4 * rho^(3/4)*n 
      opts1 = 1/rho^(3/4)
      opts2 = 1/rho^(2/4)
      opts3 = 1/rho^(1/4)
    } else {
      opt =  function(n, rho, s1, s2, s3) {
        res = n*(1/s1 + 1/s2*(1 - (1-rho)^s1) + 1/s3*(1 - (1-rho)^s2) + (1-(1-rho)^s3))
      }
      optimization = optimx(par = c(s1 = 0.1, s2 = 0.1, s3 = 0.1), fn = function(params) opt(n, rho, params["s1"], params["s2"], params["s3"]), method = c("nlminb"))
      
      if(optimization$s1 > n | optimization$value > n |  optimization$value == -Inf | optimization$convcode == 1) {
        result = n
        opts1 = 0
        opts2 = 0
        opts3 = 0
      } else {
        result = optimization$value
        opts1 = optimization$s1
        opts2 = optimization$s2
        opts3 = optimization$s3
      }
    }
    
    if(round(n * rho) == 0) {
      result = 1
    }
    
    if(result == n) {
      w1 = 24
      w2 = 0
      w3 = 0
      w4 = 0
    } else {
      w1 = 24
      w2 = 24
      w3 = 24
      w4 = 24
    }
    
    qt = (n*w1 + n*(1-(1-rho)^opts1)*w2 + n * (1-(1-rho)^opts1) * (1-(1-rho)^opts2) * w3 + n * (1-(1-rho)^opts1) * (1-(1-rho)^opts2) * (1-(1-rho)^opts3) * w4 ) / (n*24)
    
    df = data.frame("Tests" = result, "Duration" = qt)
    row.names(df) = "Four-stage"
    
    return(df)
  }
  
  Tests = rbind(individual(n),
                dorfman(n,rho),
                double(n,rho),
                triple(n,rho),
                patel3(n,rho),
                patel4(n,rho))
  
  Tests <- tibble::rownames_to_column(Tests, "Algorithm")

  return(Tests)
}


