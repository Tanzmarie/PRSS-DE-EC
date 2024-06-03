library("optimx")

calculate_tests = function(n,p,sims = 0) {
  
  individual = function(n) {
    
     if(round(n*p) == 0) {
       rs = 1
     } else {
       rs = n
     }
    
    df = data.frame("n" = n,
                    "p" = p,
                    "Theoretical" = rs,
                    "Tests" = rs,
                    "Lower" = rs,
                    "Upper" = rs,
                    "Duration" = 1)
    
    
    row.names(df) = "Individual"
    
    return(df)
  }
  
  
  
  dorfman = function(n, p, sims = 0) {
    
    # Searching for the No. Tests in Expectation
    if(round(n * p) == 0) {
      theo = 1
      opts = 0
    } else {
      if (p < 0.01) {
        opts = round(1/sqrt(p))
        theo = 2 * sqrt(p) * n
      } else {
        opt = function(n, p, s) {
          res = n * ((1/s) + 1 - (1 - p)^s)
        }
        
        optimization = optimx(par = c(s = 1), fn = function(params) opt(n, p, params["s"]), method = "nlminb")
        
        if(optimization$s > n | optimization$value > n | optimization$convcode == 1) {
          theo = n
          opts = 0
        } else {
          theo = optimization$value
          opts = round(optimization$s)
        }
      }
    }
    
    # If sims != 0 simulate the procedure
    
    if (sims != 0) {
      if(round(n * p) == 0) {
        mtests = 1
        ltests = 1
        utests = 1
      } else {
        
        num_tests_vector = numeric(sims)  # Initialize a vector to store num_tests values
        
        for (s in 1:sims) {
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
          # Save num_tests value for this iteration
          num_tests_vector[s] = num_tests
        }
        
        
        mtests = mean(num_tests_vector)
        ltests = min(num_tests_vector)
        utests = max(num_tests_vector)
        # ltests = mean(num_tests_vector) - 2.576 * sd(num_tests_vector) / sqrt(length(num_tests_vector))
        # utests = mean(num_tests_vector) + 2.576 * sd(num_tests_vector) / sqrt(length(num_tests_vector))
      }
    } else {
      mtests = NA
      ltests = NA
      utests = NA
    }
    
    # Calculate Quarantine Duration
    
    if(theo == n) {
      w1 = 24
      w2 = 0
    } else {
      w1 = 24
      w2 = 24
    }
    
    qt = (w1 + (1-(1-p)^opts)*w2) / 24
    
    df = data.frame("n" = n,
                    "p" = p,
                    "Theoretical" = theo,
                    "Tests" = mtests,
                    "Lower" = ltests,
                    "Upper" = utests,
                    "Duration" = qt)
    
    row.names(df) = "Dorfman"
    
    return(df)
  }
  
  
  grid = function(n,p, sims = 0) {
    
    # Searching for the No. Tests
    if(round(n * p) == 0) {
      theo = 1
      opts = 1
      num_tests = 1
    } else {
      if (p < 0.01) {
        opts = round(1/p^(2/3))
        theo = 3 * p^(2/3) * n
      } else {
        opt = function(n, p, s) {
          res =  n * ((2/s) + p + (1 - p) * (1 - (1 - p)^(s - 1))^2)
        }
        
        optimization = optimx(par = c(s = 1), fn = function(params) opt(n, p, params["s"]), method = "nlminb")
        
        if(optimization$s > n | optimization$value > n | optimization$convcode == 1) {
          theo = n
          opts = 1
        } else {
          theo = optimization$value
          opts = round(optimization$s)
        }
      }
    }
    
    # If sims != 0 simulate the procedure
    if (sims != 0) {
      if(round(n * p) == 0) {
        mtests = 1
        ltests = 1
        utests = 1
      } else {
        
        num_tests_vector = numeric(sims)
        
        for (s in 1:sims) {
          if ((opts - 1) != 0) {
            # State infected individuals
            infected = sample(n, size = round(p * n))
            
            # Stage 1: Divide population into random groups of size s
            shuffled_indices = sample(n)
            shuffled_indices2 = sample(n)
            num_groups = ceiling(n / opts)
            groups = split(shuffled_indices, ceiling(seq_along(1:n)/opts))
            groups2 = split(shuffled_indices2, ceiling(seq_along(1:n)/opts))
            
            
            p_groups = c()  # Initialize vector to store indices of positive groups
            p_groups2 = c() 
            
            for (i in 1:length(groups)) {
              if (sum(groups[[i]] %in% infected) > 0) {
                # If group has at least one infected individual, save its index
                p_groups = c(p_groups, i)
              }
            }
            
            for (i in 1:length(groups)) {
              if (sum(groups2[[i]] %in% infected) > 0) {
                # If group has at least one infected individual, save its index
                p_groups2 = c(p_groups2, i)
              }
            }
            
            
            
            # Stage 2: Test individuals in positive groups individually
            num_tests = 2*num_groups + sum(unlist(groups[p_groups]) %in% unlist(groups2[p_groups2]))
            
          } else {
            num_tests = n
          }
          num_tests_vector[s] = num_tests
        }
        mtests = mean(num_tests_vector)
        ltests = min(num_tests_vector)
        utests = max(num_tests_vector)
        #ltests = mean(num_tests_vector) - 2.576 * sd(num_tests_vector) / sqrt(length(num_tests_vector))
        #utests = mean(num_tests_vector) + 2.576 * sd(num_tests_vector) / sqrt(length(num_tests_vector))
      }
    } else {
      mtests = NA
      ltests = NA
      utests = NA
    }
    
    
    # Calculate the Quarantine duration
    
    if(theo == n) {
      w1 = 24
      w2 = 0
    } else {
      w1 = 24
      w2 = 24
    }
    
    qt = (w1 + (p + (1-p)*(1-(1-p)^(opts-1))^2)*w2) / 24
    
    df = data.frame("n" = n,
                    "p" = p,
                    "Theoretical" = theo,
                    "Tests" = mtests,
                    "Lower" = ltests,
                    "Upper" = utests,
                    "Duration" = qt)
    
    row.names(df) = "Double-Pooling"
    
    return(df)
  }
  
  
  triple = function(n,p, sims = 0) {
    
    # Searching for the No. Tests
    if(round(n * p) == 0) {
      theo = 1
      opts = 0
    } else {
      if (p < 0.01) {
        opts = round(p^(-3/4))
        theo = n*(p + 4*p^(3/4))
      } else {
        opt = function(n, p, s) {
          res =  n * ((3/s) + p + (1 - p) * (1 - (1 - p)^(s - 1))^3)
        }
        
        optimization = optimx(par = c(s = 1), fn = function(params) opt(n, p, params["s"]), method = "nlminb")
        
        if(optimization$s > n | optimization$value > n | optimization$convcode == 1) {
          theo = n
          opts = 1
        } else {
          theo = optimization$value
          opts = round(optimization$s)
        }
      }
    }
    
    # If sims != 0 simulate the procedure
    
    if (sims != 0) {
      if(round(n * p) == 0) {
        mtests = 1
        ltests = 1
        utests = 1
      } else {
        
        num_tests_vector = numeric(sims)  # Initialize a vector to store num_tests values
        
        for (s in 1:sims) {
          if ((opts - 1) != 0) {
            # State infected individuals
            infected = sample(n, size = round(p * n))
            
            # Stage 1: Divide population into random groups of size s
            shuffled_indices = sample(n)
            shuffled_indices2 = sample(n)
            shuffled_indices3 = sample(n)
            num_groups = ceiling(n / opts)
            groups = split(shuffled_indices, ceiling(seq_along(1:n)/opts))
            groups2 = split(shuffled_indices2, ceiling(seq_along(1:n)/opts))
            groups3 = split(shuffled_indices3, ceiling(seq_along(1:n)/opts))
            
            
            p_groups = c()  # Initialize vector to store indices of positive groups
            p_groups2 = c()
            p_groups3 = c()
            
            for (i in 1:length(groups)) {
              if (sum(groups[[i]] %in% infected) > 0) {
                # If group has at least one infected individual, save its index
                p_groups = c(p_groups, i)
              }
            }
            
            for (i in 1:length(groups2)) {
              if (sum(groups2[[i]] %in% infected) > 0) {
                # If group has at least one infected individual, save its index
                p_groups2 = c(p_groups2, i)
              }
            }
            
            for (i in 1:length(groups3)) {
              if (sum(groups3[[i]] %in% infected) > 0) {
                # If group has at least one infected individual, save its index
                p_groups3 = c(p_groups3, i)
              }
            }
            
            # Stage 2: Test individuals in positive groups individually
            num_tests = 3*num_groups + sum(unlist(groups[p_groups]) %in% unlist(groups2[p_groups2]) & unlist(groups[p_groups]) %in%  unlist(groups3[p_groups3]))
            
          } else {
            num_tests = n
          }
          # Save num_tests value for this iteration
          num_tests_vector[s] = num_tests
        }   
        
        mtests = mean(num_tests_vector)
        ltests = min(num_tests_vector)
        utests = max(num_tests_vector)
        #ltests = mean(num_tests_vector) - 2.576 * sd(num_tests_vector) / sqrt(length(num_tests_vector))
        #utests = mean(num_tests_vector) + 2.576 * sd(num_tests_vector) / sqrt(length(num_tests_vector))
      }
    } else {
      mtests = NA
      ltests = NA
      utests = NA
    }
    
    
    if(theo == n) {
      w1 = 24
      w2 = 0
    } else {
      w1 = 24
      w2 = 24
    }
    
    qt = (w1 + (p + (1-p)*(1-(1-p)^(opts-1))^3)*w2) / 24
    
    df = data.frame("n" = n,
                    "p" = p,
                    "Theoretical" = theo,
                    "Tests" = mtests,
                    "Lower" = ltests,
                    "Upper" = utests,
                    "Duration" = qt)
    
    row.names(df) = "Triple-Pooling"
    
    return(df)
  }
  
  rpooling = function (n,p,sims = 0) {
    
    opt = function(n, p, s, r) {
      res =  n * ((r/s) + p + (1 - p) * (1 - (1 - p)^(s - 1))^r)
    }
    
    optimization = optimx(par = c(s = 1, r = 1), fn = function(params) opt(n, p, params["s"], params["r"]), method = "nlminb")
    optr = round(optimization$r)
    
    if(round(n * p) == 0) {
      theo = 1
      opts = 1
      num_tests = 1
    } else {
      if (p < 0.01) {
        opts = round(p^(-optr/(optr+1)))
        theo = n*(p + (optr+1)*p^(optr/(optr+1)))
      } else {
        
        if(optimization$s > n | optimization$r < 1 | optimization$value > n | optimization$convcode == 1) {
          theo = n
          opts = 1
        } else {
          theo = optimization$value
          opts = optimization$s
        }
      }
    }
    
    # If sims != 0 simulate the procedure
    
    if (sims != 0) {
      if(round(n * p) == 0) {
        mtests = 1
        ltests = 1
        utests = 1
      } else {
        
        num_tests_vector = numeric(sims)  # Initialize a vector to store num_tests values
        
        for (s in 1:sims) {
          if ((opts - 1) != 0 & optr > 1) {
            # State infected individuals
            infected = sample(n, size = round(p * n))
            
            # Stage 1: Divide population into random groups of size s
            
            groups_list <- list()
            
            # Generate and split shuffled indices for each set
            num_groups = ceiling(n / opts)
            
            for (i in 1:optr) {
              shuffled_indices <- sample(n)
              groups <- split(shuffled_indices, ceiling(seq_along(1:n) / opts))
              groups_list[[i]] <- groups
            }
            
            
            p_groups_list = list()  # Initialize vector to store indices of positive groups
            
            
            for(i in 1:optr) {
              p_groups = c()
              for (j in 1:length(groups_list[[i]])) {
                if (sum(groups_list[[i]][[j]] %in% infected) > 0) {
                  # If group has at least one infected individual, save its index
                  p_groups = c(p_groups, j)
                }
                p_groups_list[[i]] = p_groups
              }
            }
            
            
            # Stage 2: Test individuals in positive groups individually
            ps = list()
            for(i in 1:optr) {
              ps[[i]] = unlist(unname(groups_list[[i]][p_groups_list[[i]]]))
            }
            
            
            # Initialize the intersection with the first list
            common_elements <- ps[[1]]
            
            # Iterate through each list starting from the second one
            
            if(optr == 1) {
              common_elements = ps[[1]]
            } else {
              common_elements = ps[[1]]
              for (i in 2:length(ps)) {
                # Find the intersection between the current common elements and the next list
                common_elements <- intersect(common_elements, ps[[i]])
              }
            }
            
            num_tests = optr*num_groups + length(common_elements)
            
            
          } else {
            num_tests = n
          }
          # Save num_tests value for this iteration
          num_tests_vector[s] = num_tests
        }   
        
        mtests = mean(num_tests_vector)
        ltests = min(num_tests_vector)
        utests = max(num_tests_vector)
        #ltests = mean(num_tests_vector) - 2.576 * sd(num_tests_vector) / sqrt(length(num_tests_vector))
        #utests = mean(num_tests_vector) + 2.576 * sd(num_tests_vector) / sqrt(length(num_tests_vector))
      }
    } else {
      mtests = NA
      ltests = NA
      utests = NA
    }
    
    if(theo == n) {
      w1 = 24
      w2 = 0
    } else {
      w1 = 24
      w2 = 24
    }
    
    if (round(n*p) == 0 | theo == n) {
      qt = 1
    } else {
      qt = (w1 + (p + (1-p)*(1-(1-p)^(opts-1))^optr)*w2) / 24
    }
    
    df = data.frame("n" = n,
                    "p" = p,
                    "Theoretical" = theo,
                    "Tests" = mtests,
                    "Lower" = ltests,
                    "Upper" = utests,
                    "Duration" = qt)
    
    row.names(df) = "R-Pooling"
    
    return(df)
  }
  
  
  three = function(n, p, sims = 0) {
    
    # Searching for the No. Tests
    if(round(n * p) == 0) {
      theo = 1
      opts1 = 0
      opts2 = 0
    } else {
      if (p < 0.01) {
        theo = 3*p^(2/3)*n 
        opts1 = round(1/p^(2/3))
        opts2 = round(1/p^(1/3))
      } else {
        opt =  function(n, p, s1, s2) {
          res = n*(1/s1 + 1/s2*(1 - (1-p)^s1) + (1 - (1-p)^s2))
        }
        
        optimization = optimx(par = c(s1 = 1, s2 = 1), fn = function(params) opt(n, p, params["s1"], params["s2"] ), method = c("nlminb"))
        
        
        if(optimization$s1 > n | optimization$value > n | optimization$value == -Inf | optimization$convcode == 1) {
          theo = n
          opts1 = 0
          opts2 = 0
        } else {
          theo = optimization$value
          opts1 = optimization$s1
          opts2 = optimization$s2
        }
      }
    }
    
    if (sims != 0) {
      if(round(n * p) == 0) {
        mtests = 1
        ltests = 1
        utests = 1
      } else {
        
        num_tests_vector = numeric(sims)  # Initialize a vector to store num_tests values
        
        # Simulate the 3-Stage procedure
        for(s in 1:sims) {
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
          # Save num_tests value for this iteration
          num_tests_vector[s] = num_tests
        }
        
        mtests = mean(num_tests_vector)
        ltests = min(num_tests_vector)
        utests = max(num_tests_vector)
        #ltests = mean(num_tests_vector) - 2.576 * sd(num_tests_vector) / sqrt(length(num_tests_vector))
        #utests = mean(num_tests_vector) + 2.576 * sd(num_tests_vector) / sqrt(length(num_tests_vector))
      }
    } else {
      mtests = NA
      ltests = NA
      utests = NA
    }
    
    
    
    if(theo == n) {
      w1 = 24
      w2 = 0
      w3 = 0
    } else {
      w1 = 24
      w2 = 24
      w3 = 24
    }
    
    qt = (w1 + (1-(1-p)^opts1)* w2 +  (1-(1-p)^opts2) * w3) / 24
    
    df = data.frame("n" = n,
                    "p" = p,
                    "Theoretical" = theo,
                    "Tests" = mtests,
                    "Lower" = ltests,
                    "Upper" = utests,
                    "Duration" = qt)
    
    row.names(df) = "3-Stage"
    
    return(df)
  }
  
  four = function(n, p, sims = 0) {
    
    if(round(n * p) == 0) {
      theo = 1
      opts1 = 0
      opts2 = 0
      opts3 = 0
    } else {
      # Searching for the No. Tests in Expectation
      if (p < 0.01) {
        theo = 4 * p^(3/4)*n 
        opts1 = 1/p^(3/4)
        opts2 = 1/p^(2/4)
        opts3 = 1/p^(1/4)
      } else {
        opt =  function(n, p, s1, s2, s3) {
          res = n*(1/s1 + 1/s2*(1 - (1-p)^s1) + 1/s3*(1 - (1-p)^s2) + (1-(1-p)^s3))
        }
        
        optimization = optimx(par = c(s1 = 1, s2 = 1, s3 = 1), fn = function(params) opt(n, p, params["s1"], params["s2"], params["s3"]), method = c("nlminb"))
        
        if(optimization$s1 > n | optimization$value > n | optimization$value == -Inf | optimization$convcode == 1) {
          theo = n
          opts1 = 0
          opts2 = 0
          opts3 = 0
        } else {
          theo = optimization$value
          opts1 = optimization$s1
          opts2 = optimization$s2
          opts3 = optimization$s3
        }
      }
    }
    
    
    
    if (sims != 0) {
      if(round(n * p) == 0) {
        mtests = 1
        ltests = 1
        utests = 1
      } else {
        
        num_tests_vector = numeric(sims)  # Initialize a vector to store num_tests values
        
        for (s in 1:sims) {
          # Simulate the Dorfman procedure
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
            
            # Stage 3: Divide positive pools in subpools
            n3 = as.numeric(length(unlist(groups2[p_groups2])))
            shuffled_indices3 = unname(sample(unlist(groups2[p_groups2])))
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
          
          # Save num_tests value for this iteration
          num_tests_vector[s] = num_tests
        }
        
        mtests = mean(num_tests_vector)
        ltests = min(num_tests_vector)
        utests = max(num_tests_vector)
        #ltests = mean(num_tests_vector) - 2.576 * sd(num_tests_vector) / sqrt(length(num_tests_vector))
        #utests = mean(num_tests_vector) + 2.576 * sd(num_tests_vector) / sqrt(length(num_tests_vector))
      }
    } else {
      mtests = NA
      ltests = NA
      utests = NA
    }
    
    
    if(theo == n) {
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
    
    qt = (w1 + (1-(1-p)^opts1)*w2 + (1-(1-p)^opts2) * w3 + (1-(1-p)^opts3) * w4 ) / 24
    
    
    df = data.frame("n" = n,
                    "p" = p,
                    "Theoretical" = theo,
                    "Tests" = mtests,
                    "Lower" = ltests,
                    "Upper" = utests,
                    "Duration" = qt)
    
    row.names(df) = "4-Stage"
    
    return(df)
  }
  
  Tests = rbind(individual(n),
                dorfman(n,p,sims),
                #grid(n,p,sims),
                #rpooling(n,p,sims),
                three(n,p, sims),
                four(n,p, sims))
  
  Tests = tibble::rownames_to_column(Tests, "Algorithm")
  
  return(Tests)
  
}

