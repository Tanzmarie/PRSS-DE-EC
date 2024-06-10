library("optimx")
library("foreach")
library("doParallel")

calculate_tests = function(n, p, sims = 0) {
  
  one = function(n, p) {
    
    res = as.numeric(ifelse(round(n * p) == 0, 1, n))
    
    df = data.frame(
      "n" = n,
      "p" = p,
      "Theoretical" = res,
      "Tests" = res,
      "Lower" = res,
      "Upper" = res,
      "Duration" = 1
    )
    
    row.names(df) = "One-stage"
    
    return(df)
  }
  
  two = function(n, p, sims = 0) {
    # Searching for the No. Tests in Expectation
    if (round(n * p) == 0) {
      theo = 1
      opts = 0
      w1 = 1
      w2 = 1
    } else {
      opt = function(n, p, s) {
        res = n * ((1/s) + 1 - (1 - p)^s)
      }
      
      optimization = optimx(par = c(s = 1), fn = function(params) opt(n, p, params["s"]), method = "nlminb")
      
      if (optimization$s > n | optimization$value > n | optimization$convcode == 1) {
        theo = n
        opts = 0
        w1 = 1
        w2 = 0
      } else {
        theo = optimization$value
        opts = optimization$s
        w1 = 1
        w2 = 1
      }
    }
    
    # Calculate Duration
    qt = w1 + (1-(1-p)^opts)*w2
    
    # If sims != 0 simulate the procedure
    if (sims != 0) {
      if (round(n * p) == 0) {
        mtests = 1
        ltests = 1
        utests = 1
      } else {
        # Use foreach to parallelize the loop
        num_tests_vector = foreach(s = 1:sims, .combine = c) %dopar% {
          # Simulate the Dorfman procedure
          if (opts != 0) {
            # State infected individuals
            infected = sample(n, size = round(p * n))
            
            # Stage 1: Divide population into random groups of size s
            shuffled_indices = sample(n)
            num_groups = ceiling(n / opts)
            groups = split(shuffled_indices, ceiling(seq_along(1:n)/opts))
            
            p_groups = c()  # Initialize vector to store indices of positive groups
            
            for (i in 1:length(groups)) {
              if (sum(groups[[i]] %in% infected) > 0) {
                # If group has at least one infected individual, save its index
                p_groups = c(p_groups, i)
              }
            }
            
            # Stage 2: Test individuals in positive groups individually
            num_tests = num_groups + length(p_groups) * opts
            
          } else {
            num_tests = n
          }
          
          # Return num_tests value for this iteration
          return(num_tests)
        }
        
        mtests = mean(num_tests_vector)
        ltests = min(num_tests_vector)
        utests = max(num_tests_vector)
      }
    } else {
      mtests = NA
      ltests = NA
      utests = NA
    }
    
    df = data.frame("n" = n,
                    "p" = p,
                    "Theoretical" = theo,
                    "Tests" = mtests,
                    "Lower" = ltests,
                    "Upper" = utests,
                    "Duration" = qt)
    
    row.names(df) = "Two-stage"
    
    return(df)
  }

  three = function(n, p, sims = 0) {
    
    # Searching for the No. Tests
    if(round(n * p) == 0) {
      theo = 1
      opts1 = 0
      opts2 = 0
      w1 = 1
      w2 = 0
      w3 = 0
    } else {
      opt =  function(n, p, s1, s2) {
        res = n*(1/s1 + 1/s2*(1 - (1-p)^s1) + (1 - (1-p)^s2))
      }
      
      optimization = optimx(par = c(s1 = 1, s2 = 1), fn = function(params) opt(n, p, params["s1"], params["s2"] ), method = c("L-BFGS-B"), lower = c(1,1))
      
      
      if(optimization$s1 > n | optimization$value > n | optimization$value == -Inf | optimization$convcode == 1) {
        theo = n
        opts1 = 0
        opts2 = 0
        w1 = 1
        w2 = 0
        w3 = 0
      } else {
        theo = optimization$value
        opts1 = optimization$s1
        opts2 = optimization$s2
        w1 = 1
        w2 = 1
        w3 = 1
      }
    }
    
    # Calculate Duration
    
    qt = w1 + (1-(1-p)^opts1)* w2 +  (1-(1-p)^opts2) * w3
    
    # Simulate Procedure 
    
    if (sims != 0) {
      if(round(n * p) == 0) {
        mtests = 1
        ltests = 1
        utests = 1
      } else {
        
        num_tests_vector = foreach(s = 1:sims, .combine = c) %dopar% {
          if (opts1 != 0 & opts2 != 0) {
            # State infected individuals
            infected = sample(n, size = round(p * n))
            
            # Stage 1: Divide population into random groups of size s
            shuffled_indices = sample(n)
            num_groups = ceiling(n / opts1)
            groups = split(shuffled_indices, ceiling(seq_along(1:n)/opts1))
            
            
            p_groups = c()  # Initialize vector to store indices of positive groups
            
            for (i in 1:length(groups)) {
              if (sum(groups[[i]] %in% infected) > 0) {
                # If group has at least one infected individual, save its index
                p_groups = c(p_groups, i)
              }
            }
            
            # Stage 2: Divide positive pools in subpools
            n2 = as.numeric(length(unlist(groups[p_groups])))
            shuffled_indices2 = unname(sample(unlist(groups[p_groups])))
            num_groups2 = ceiling(n2/opts2)
            groups2 = split(shuffled_indices2, ceiling(seq_along(1:n2)/opts2))
            
            
            p_groups2 = c()  # Initialize vector to store indices of positive groups
            
            for (i in 1:length(groups2)) {
              if (sum(groups2[[i]] %in% infected) > 0) {
                # If group has at least one infected individual, save its index
                p_groups2 = c(p_groups2, i)
              }
            }
            
            # Stage 3: Test individuals in positive groups individually
            num_tests = num_groups + num_groups2 + length(p_groups2) * opts2
            
          } else  {
            num_tests = n
          }
          return(num_tests)
        }
        
        mtests = mean(num_tests_vector)
        ltests = min(num_tests_vector)
        utests = max(num_tests_vector)
        
      }
    } else {
      mtests = NA
      ltests = NA
      utests = NA
    }
    
    
    df = data.frame("n" = n,
                    "p" = p,
                    "Theoretical" = theo,
                    "Tests" = mtests,
                    "Lower" = ltests,
                    "Upper" = utests,
                    "Duration" = qt)
    
    row.names(df) = "Three-stage"
    
    return(df)
  }
  
  four = function(n, p, sims = 0) {
    
    if(round(n * p) == 0) {
      theo = 1
      opts1 = 0
      opts2 = 0
      opts3 = 0
      w1 = 1
      w2 = 0
      w3 = 0
      w4 = 0
    } else {
      opt =  function(n, p, s1, s2, s3) {
        res = n*(1/s1 + 1/s2*(1 - (1-p)^s1) + 1/s3*(1 - (1-p)^s2) + (1-(1-p)^s3))
      }
      
      optimization = optimx(par = c(s1 = 1, s2 = 1, s3 = 1), fn = function(params) opt(n, p, params["s1"], params["s2"], params["s3"]), method = c("L-BFGS-B"), lower = c(1,1,1))
      
      if(optimization$s1 > n | optimization$value > n | optimization$value == -Inf | optimization$convcode == 1) {
        theo = n
        opts1 = 0
        opts2 = 0
        opts3 = 0
        w1 = 1
        w2 = 0
        w3 = 0
        w4 = 0
      } else {
        theo = optimization$value
        opts1 = optimization$s1
        opts2 = optimization$s2
        opts3 = optimization$s3
        w1 = 1
        w2 = 1
        w3 = 1
        w4 = 1
      }
    }
    
    qt = w1 + (1-(1-p)^opts1)*w2 + (1-(1-p)^opts2) * w3 + (1-(1-p)^opts3) * w4 
    
    
    if (sims != 0) {
      if(round(n * p) == 0) {
        mtests = 1
        ltests = 1
        utests = 1
      } else {
        
        num_tests_vector = foreach(s = 1:sims, .combine = c) %dopar% {
          # Simulate the procedure
          if (opts1 > 0 & opts2 > 0 & opts3 > 0) {
            # State infected individuals
            infected = sample(n, size = round(p * n))
            
            # Stage 1: Divide population into random groups of size s
            shuffled_indices = sample(n)
            num_groups = ceiling(n / opts1)
            groups = split(shuffled_indices, ceiling(seq_along(1:n)/opts1))
            
            
            p_groups = c()  # Initialize vector to store indices of positive groups
            
            for (i in 1:length(groups)) {
              if (sum(groups[[i]] %in% infected) > 0) {
                # If group has at least one infected individual, save its index
                p_groups = c(p_groups, i)
              }
            }
            
            # Stage 2: Divide positive pools in subpools
            n2 = as.numeric(length(unlist(groups[p_groups])))
            shuffled_indices2 = unname(unlist(groups[p_groups]))
            num_groups2 = ceiling(n2/opts2)
            groups2 = split(shuffled_indices2, ceiling(seq_along(1:n2)/opts2))
            
            
            p_groups2 = c()  # Initialize vector to store indices of positive groups
            
            for (i in 1:length(groups2)) {
              if (sum(groups2[[i]] %in% infected) > 0) {
                # If group has at least one infected individual, save its index
                p_groups2 = c(p_groups2, i)
              }
            }
            
            # Stage 3: Divide positive pools in subpools
            n3 = as.numeric(length(unlist(groups2[p_groups2])))
            shuffled_indices3 = unname(unlist(groups2[p_groups2]))
            num_groups3 = ceiling(n3/opts3)
            groups3 = split(shuffled_indices3, ceiling(seq_along(1:n3)/opts3))
            
            
            p_groups3 = c()  # Initialize vector to store indices of positive groups
            
            for (i in 1:length(groups3)) {
              if (sum(groups3[[i]] %in% infected) > 0) {
                # If group has at least one infected individual, save its index
                p_groups3 = c(p_groups3, i)
              }
            }
            
            
            # Stage 4: Test individuals in positive groups individually
            num_tests = num_groups + num_groups2 + num_groups3 + length(p_groups3) * opts3
            
          } else  {
            num_tests = n
          }
          
          return(num_tests)
        }
        
        mtests = mean(num_tests_vector)
        ltests = min(num_tests_vector)
        utests = max(num_tests_vector)
      }
    } else {
      mtests = NA
      ltests = NA
      utests = NA
    }
    
    
    
    df = data.frame("n" = n,
                    "p" = p,
                    "Theoretical" = theo,
                    "Tests" = mtests,
                    "Lower" = ltests,
                    "Upper" = utests,
                    "Duration" = qt)
    
    row.names(df) =  "Four-Stage"
    
    return(df)
  }
  
  five = function(n, p, sims = 0) {
    
    if(round(n * p) == 0) {
      theo = 1
      opts1 = 0
      opts2 = 0
      opts3 = 0
      opts4 = 0
      w1 = 1
      w2 = 0
      w3 = 0
      w4 = 0
      w5 = 0
    } else {
      opt =  function(n, p, s1, s2, s3, s4) {
        res = n*(1/s1 + 1/s2*(1 - (1-p)^s1) + 1/s3*(1 - (1-p)^s2) + 1/s4*(1-(1-p)^s3) + (1-(1-p)^s4))
      }
      
      optimization = optimx(par = c(s1 = 1, s2 = 1, s3 = 1, s4 = 1), fn = function(params) opt(n, p, params["s1"], params["s2"], params["s3"], params["s4"]), method = c("L-BFGS-B"), lower = c(1,1,1,1))
      
      if(optimization$s1 > n | optimization$value > n | optimization$value == -Inf | optimization$convcode == 1) {
        theo = n
        opts1 = 0
        opts2 = 0
        opts3 = 0
        opts4 = 0
        w1 = 1
        w2 = 0
        w3 = 0
        w4 = 0
        w5 = 0
      } else {
        theo = optimization$value
        opts1 = optimization$s1
        opts2 = optimization$s2
        opts3 = optimization$s3
        opts4 = optimization$s4
        w1 = 1
        w2 = 1
        w3 = 1
        w4 = 1
        w5 = 1
      }
    }
    
    qt = w1 + (1-(1-p)^opts1)*w2 + (1-(1-p)^opts2) * w3 + (1-(1-p)^opts3) * w4 + (1-(1-p)^opts3) * w5
    
    
    if (sims != 0) {
      if(round(n * p) == 0) {
        mtests = 1
        ltests = 1
        utests = 1
      } else {
        
        num_tests_vector = foreach(s = 1:sims, .combine = c) %dopar% {
          # Simulate the procedure
          if (opts1 > 0 & opts2 > 0 & opts3 > 0 & opts4 > 0) {
            # State infected individuals
            infected = sample(n, size = round(p * n))
            
            # Stage 1: Divide population into random groups of size s
            shuffled_indices = sample(n)
            num_groups = ceiling(n / opts1)
            groups = split(shuffled_indices, ceiling(seq_along(1:n)/opts1))
            
            
            p_groups = c()  # Initialize vector to store indices of positive groups
            
            for (i in 1:length(groups)) {
              if (sum(groups[[i]] %in% infected) > 0) {
                # If group has at least one infected individual, save its index
                p_groups = c(p_groups, i)
              }
            }
            
            # Stage 2: Divide positive pools in subpools
            n2 = as.numeric(length(unlist(groups[p_groups])))
            shuffled_indices2 = unname(unlist(groups[p_groups]))
            num_groups2 = ceiling(n2/opts2)
            groups2 = split(shuffled_indices2, ceiling(seq_along(1:n2)/opts2))
            
            
            p_groups2 = c()  # Initialize vector to store indices of positive groups
            
            for (i in 1:length(groups2)) {
              if (sum(groups2[[i]] %in% infected) > 0) {
                # If group has at least one infected individual, save its index
                p_groups2 = c(p_groups2, i)
              }
            }
            
            # Stage 3: Divide positive pools in subpools
            n3 = as.numeric(length(unlist(groups2[p_groups2])))
            shuffled_indices3 = unname(unlist(groups2[p_groups2]))
            num_groups3 = ceiling(n3/opts3)
            groups3 = split(shuffled_indices3, ceiling(seq_along(1:n3)/opts3))
            
            
            p_groups3 = c()  # Initialize vector to store indices of positive groups
            
            for (i in 1:length(groups3)) {
              if (sum(groups3[[i]] %in% infected) > 0) {
                # If group has at least one infected individual, save its index
                p_groups3 = c(p_groups3, i)
              }
            }
            
            # Stage 4: Divide positive pools in subpools
            n4 = as.numeric(length(unlist(groups3[p_groups3])))
            shuffled_indices4 = unname(unlist(groups3[p_groups3]))
            num_groups4 = ceiling(n4/opts4)
            groups4 = split(shuffled_indices4, ceiling(seq_along(1:n4)/opts4))
            
            
            p_groups4 = c()  # Initialize vector to store indices of positive groups
            
            for (i in 1:length(groups4)) {
              if (sum(groups4[[i]] %in% infected) > 0) {
                # If group has at least one infected individual, save its index
                p_groups4 = c(p_groups4, i)
              }
            }
            
            
            # Stage 5: Test individuals in positive groups individually
            num_tests = num_groups + num_groups2 + num_groups3 + num_groups4 + length(p_groups4) * opts4
            
          } else  {
            num_tests = n
          }
          
          return(num_tests)
        }
        
        mtests = mean(num_tests_vector)
        ltests = min(num_tests_vector)
        utests = max(num_tests_vector)
      }
    } else {
      mtests = NA
      ltests = NA
      utests = NA
    }
    
    
    
    df = data.frame("n" = n,
                    "p" = p,
                    "Theoretical" = theo,
                    "Tests" = mtests,
                    "Lower" = ltests,
                    "Upper" = utests,
                    "Duration" = qt)
    
    row.names(df) = "Five-Stage"
    
    return(df)
  }
  
  Tests = rbind(one(n,p),
                two(n,p,sims),
                three(n,p, sims),
                four(n,p, sims),
                five(n,p,sims))
  
  Tests = tibble::rownames_to_column(Tests, "Algorithm")
  
  return(Tests)
  
}

