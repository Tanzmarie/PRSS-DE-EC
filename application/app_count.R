library("readr")
library("tidyverse")
library("optimx")
library("furrr")
library("foreach")
library("doFuture")

econ = function(n, p, cf, cv, cl, h, tau0, data, sims) {
  
  one = function(n, p, cf, cv, cl, h, tau0) {
    
    res = as.numeric(ifelse(round(n * p) == 0, 1, n))
    
    if (res == 1) {
      TotalCosts = NA
    } else {
      # Deterministic costs
      DC = ifelse(tau0 < res, cf + tau0 * cv, cf + res * cv)
      
      
      F1 = sample(data, size = n, replace = TRUE)
      # Stochastic costs
      CS = (1-h) * sum(F1)
      
      # Outsource cost
      CO = ifelse(tau0 < res, (res - tau0) * cl, 0)
      
      # Total costs
      TotalCosts = (DC + CS + CO)
    }
    
    
    df = data.frame(
      "n" = n,
      "p" = p,
      "Theoretical" = res / n,
      "Tests" = res / n,
      "LowTests" = res / n,
      "UpTests" = res / n,
      "MCosts" = TotalCosts / n,
      "LCosts" = TotalCosts / n,
      "UCosts" = TotalCosts / n
    )
    
    row.names(df) = "One-stage"
    
    return(df)
  }
  
  
  two = function(n, p, cf, cv, cl, h, tau0, sims = 0) {
    # Searching for the No. Tests in Expectation
    if (round(n * p) == 0) {
      theo = 1
      opts = 0
    } else {
      opt = function(n, p, s) {
        res = n * ((1/s) + 1 - (1 - p)^s)
      }
      
      optimization = optimx(par = c(s = 1), fn = function(params) opt(n, p, params["s"]), method = "nlminb")
      
      if (optimization$s > n | optimization$value > n | optimization$convcode == 1) {
        theo = n
        opts = 0
      } else {
        theo = optimization$value
        opts = optimization$s
      }
    }
    
    
    # If sims != 0 simulate the procedure
    if (sims != 0) {
      if (round(n * p) == 0) {
        mtests = 1
        ltests = 1
        utests = 1
        mcosts = NA
        lcosts = NA
        ucosts = NA
      } else {
        
        num_tests_costs_matrix = foreach(s = 1:sims, .combine = rbind, .options.future = list(seed = TRUE)) %dofuture%  {
          
          
          if (opts != 0) {
            # State infected individuals
            infected = sample(n, size = round(p * n))
            
            # Stage 1: Divide population into random groups of size opts
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
            
            
            # Calculate costs
            
            # Deterministic costs
            DC = ifelse(tau0 < num_tests, cf + tau0 * cv, cf + num_tests * cv)
            
            
            F1 = sample(data, size = n, replace = TRUE)
            F2 = sample(F1, length(p_groups) * opts)
            # Stochastic costs
            CS = (1-h) * (sum(F1) + sum(F2))
            
            # Outsource cost
            CO = ifelse(tau0 < num_tests, (num_tests - tau0) * cl, 0)
            
            # Total costs
            TotalCosts = (DC + CS + CO)
            
            
          } else {
            num_tests = n
            TotalCosts = NA
          }
          
          # Return both num_tests and duration for this iteration
          return(c(num_tests, TotalCosts))
        }
        
        # Separate the results into num_tests_vector and num_dur_vector
        num_tests_vector = num_tests_costs_matrix[, 1]
        num_cost_vector = num_tests_costs_matrix[, 2]
        
        # Calculate statistics
        mtests = mean(num_tests_vector)
        ltests = min(num_tests_vector)
        utests = max(num_tests_vector)
        
        
        mcosts = mean(num_cost_vector)
        lcosts = min(num_cost_vector)
        ucosts = max(num_cost_vector)
        
      }
    } else {
      mtests = NA
      ltests = NA
      utests = NA
      mcosts = NA
      lcosts = NA
      ucosts = NA
    }
    
    
    df = data.frame("n" = n,
                    "p" = p,
                    "Theoretical" = theo / n,
                    "Tests" = mtests / n,
                    "LowTests" = ltests / n,
                    "UpTests" = utests / n,
                    "MCosts" = mcosts / n,
                    "LCosts" = lcosts / n,
                    "UCosts" = ucosts / n)
    
    row.names(df) = "Two-stage"
    
    return(df)
  }
  
  three = function(n, p, cf, cv, cl, h, tau0, sims = 0) {
    
    # Searching for the No. Tests
    if(round(n * p) == 0) {
      theo = 1
      opts1 = 0
      opts2 = 0
    } else {
      opt =  function(n, p, s1, s2) {
        res = n*(1/s1 + 1/s2*(1 - (1-p)^s1) + (1 - (1-p)^s2))
      }
      
      optimization = optimx(par = c(s1 = 1, s2 = 1), fn = function(params) opt(n, p, params["s1"], params["s2"] ), method = c("L-BFGS-B"), lower = c(1,1))
      
      
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
    
    
    # Simulate Procedure 
    
    if (sims != 0) {
      if(round(n * p) == 0) {
        mtests = 1
        ltests = 1
        utests = 1
        mcosts = NA
        lcosts = NA
        ucosts = NA
      } else {
        
        num_tests_costs_matrix = foreach(s = 1:sims, .combine = rbind, .options.future = list(seed = TRUE)) %dofuture%  {
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
            
            # Stage 3: Test individuals in positive groups individually
            num_tests = num_groups + num_groups2 + length(p_groups2) * opts2
            
            # Calculate costs
            
            # Deterministic costs
            DC = ifelse(tau0 < num_tests, cf + tau0 * cv, cf + num_tests * cv)
            
            
            F1 = sample(data, size = n, replace = TRUE)
            F2 = sample(F1, length(p_groups) * opts1)
            F3 = sample(F2, length(p_groups2) * opts2)
            
            # Stochastic costs
            CS = (1-h) * (sum(F1) + sum(F2) + sum(F3))
            
            # Outsource cost
            CO = ifelse(tau0 < num_tests, (num_tests - tau0) * cl, 0)
            
            # Total costs
            TotalCosts = (DC + CS + CO)
            
            
            
            
            
          } else {
            num_tests = n
            TotalCosts = NA
          }
          
          # Return both num_tests and duration for this iteration
          return(c(num_tests, TotalCosts))
        }
        
        # Separate the results into num_tests_vector and num_dur_vector
        num_tests_vector = num_tests_costs_matrix[, 1]
        num_cost_vector = num_tests_costs_matrix[, 2]
        
        # Calculate statistics
        mtests = mean(num_tests_vector)
        ltests = min(num_tests_vector)
        utests = max(num_tests_vector)
        
        
        mcosts = mean(num_cost_vector)
        lcosts = min(num_cost_vector)
        ucosts = max(num_cost_vector)
        
      }
    } else {
      mtests = NA
      ltests = NA
      utests = NA
      mcosts = NA
      lcosts = NA
      ucosts = NA
    }
    
    
    df = data.frame("n" = n,
                    "p" = p,
                    "Theoretical" = theo / n,
                    "Tests" = mtests / n,
                    "LowTests" = ltests / n,
                    "UpTests" = utests / n,
                    "MCosts" = mcosts / n,
                    "LCosts" = lcosts / n,
                    "UCosts" = ucosts / n)
    
    row.names(df) = "Three-stage"
    
    return(df)
  }
  
  four = function(n, p, cf, cv, cl, h, tau0, sims = 0) {
    
    if(round(n * p) == 0) {
      theo = 1
      opts1 = 0
      opts2 = 0
      opts3 = 0
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
      } else {
        theo = optimization$value
        opts1 = optimization$s1
        opts2 = optimization$s2
        opts3 = optimization$s3
      }
    }
    
    
    
    if (sims != 0) {
      if(round(n * p) == 0) {
        mtests = 1
        ltests = 1
        utests = 1
        mcosts = NA
        lcosts = NA
        ucosts = NA
      } else {
        
        num_tests_costs_matrix = foreach(s = 1:sims, .combine = rbind, .options.future = list(seed = TRUE)) %dofuture%  {
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
            
            # Calculate costs
            
            # Deterministic costs
            DC = ifelse(tau0 < num_tests, cf + tau0 * cv, cf + num_tests * cv)
            
            
            F1 = sample(data, size = n, replace = TRUE)
            F2 = sample(F1, length(p_groups) * opts1)
            F3 = sample(F2, length(p_groups2) * opts2)
            F4 = sample(F3, length(p_groups3) * opts3)
            
            # Stochastic costs
            CS = (1-h) * (sum(F1) + sum(F2) + sum(F3) + sum(F4))
            
            # Outsource cost
            CO = ifelse(tau0 < num_tests, (num_tests - tau0) * cl, 0)
            
            # Total costs
            TotalCosts = (DC + CS + CO)
            
            
            
          } else {
            num_tests = n
            TotalCosts = NA
          }
          
          # Return both num_tests and duration for this iteration
          return(c(num_tests, TotalCosts))
        }
        
        # Separate the results into num_tests_vector and num_dur_vector
        num_tests_vector = num_tests_costs_matrix[, 1]
        num_cost_vector = num_tests_costs_matrix[, 2]
        
        # Calculate statistics
        mtests = mean(num_tests_vector)
        ltests = min(num_tests_vector)
        utests = max(num_tests_vector)
        
        
        mcosts = mean(num_cost_vector)
        lcosts = min(num_cost_vector)
        ucosts = max(num_cost_vector)
      }
    } else {
      mtests = NA
      ltests = NA
      utests = NA
      mcosts = NA
      lcosts = NA
      ucosts = NA
    }
    
    
    
    df = data.frame("n" = n,
                    "p" = p,
                    "Theoretical" = theo / n,
                    "Tests" = mtests / n,
                    "LowTests" = ltests / n,
                    "UpTests" = utests / n,
                    "MCosts" = mcosts / n,
                    "LCosts" = lcosts / n,
                    "UCosts" = ucosts / n)
    
    row.names(df) =  "Four-stage"
    
    return(df)
  }
  
  five = function(n, p, cf, cv, cl, h, tau0, sims = 0) {

    if(round(n * p) == 0) {
      theo = 1
      opts1 = 0
      opts2 = 0
      opts3 = 0
      opts4 = 0
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
      } else {
        theo = optimization$value
        opts1 = optimization$s1
        opts2 = optimization$s2
        opts3 = optimization$s3
        opts4 = optimization$s4
      }
    }


    if (sims != 0) {
      if(round(n * p) == 0) {
        mtests = 1
        ltests = 1
        utests = 1
        mcosts = NA
        lcosts = NA
        ucosts = NA
      } else {

        num_tests_costs_matrix = foreach(s = 1:sims, .combine = rbind, .options.future = list(seed = TRUE)) %dofuture%  {
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

            # Calculate costs

            # Deterministic costs
            DC = ifelse(tau0 < num_tests, cf + tau0 * cv, cf + num_tests * cv)


            F1 = sample(data, size = n, replace = TRUE)
            F2 = sample(F1, length(p_groups) * opts1)
            F3 = sample(F2, length(p_groups2) * opts2)
            F4 = sample(F3, length(p_groups3) * opts3)
            F5 = sample(F4, length(p_groups4) * opts4)

            # Stochastic costs
            CS = (1-h) * (sum(F1) + sum(F2) + sum(F3) + sum(F4) + sum(F5))

            # Outsource cost
            CO = ifelse(tau0 < num_tests, (num_tests - tau0) * cl, 0)

            # Total costs
            TotalCosts = (DC + CS + CO)


          } else {
            num_tests = n
            TotalCosts = NA
          }

          # Return both num_tests and duration for this iteration
          return(c(num_tests, TotalCosts))
        }

        # Separate the results into num_tests_vector and num_dur_vector
        num_tests_vector = num_tests_costs_matrix[, 1]
        num_cost_vector = num_tests_costs_matrix[, 2]

        # Calculate statistics
        mtests = mean(num_tests_vector)
        ltests = min(num_tests_vector)
        utests = max(num_tests_vector)


        mcosts = mean(num_cost_vector)
        lcosts = min(num_cost_vector)
        ucosts = max(num_cost_vector)
      }
    } else {
      mtests = NA
      ltests = NA
      utests = NA
      mcosts = NA
      lcosts = NA
      ucosts = NA
    }



    df = data.frame("n" = n,
                    "p" = p,
                    "Theoretical" = theo / n,
                    "Tests" = mtests / n,
                    "LowTests" = ltests / n,
                    "UpTests" = utests / n,
                    "MCosts" = mcosts / n,
                    "LCosts" = lcosts / n,
                    "UCosts" = ucosts / n)

    row.names(df) = "Five-stage"

    return(df)
  }
  
  Costs = rbind(one(n,p,cf,cv,cl,h,tau0),
                two(n,p,cf,cv,cl,h,tau0, sims),
                three(n,p,cf,cv,cl,h,tau0, sims),
                four(n,p,cf,cv,cl,h,tau0, sims),
                five(n,p,cf,cv,cl,h,tau0, sims))
  
  Costs = tibble::rownames_to_column(Costs, "Algorithm")
  
  return(Costs)
  
}

# Load COVID-19 data for Germany
covid = read_csv("application/data/COVID-19-Faelle_7-Tage-Inzidenz_Landkreise.csv")
covidhb = covid[which(covid$Landkreis_id == "02000"),]
covidbr = covid[which(covid$Landkreis_id == "04011"),]
covidbe = covid[which(covid$Landkreis_id == "11001"),]

# Estimate the point-prevalence
covidhb$prevalence = ((covidhb$`Inzidenz_7-Tage`/7) * 14)/100000
covidbr$prevalence = ((covidbr$`Inzidenz_7-Tage`/7) * 14)/100000
covidbe$prevalence = ((covidbe$`Inzidenz_7-Tage`/7) * 14)/100000

prevalencehb = sort(unique(covidhb$prevalence))
prevalencebr = sort(unique(covidbr$prevalence))
prevalencebe = sort(unique(covidbe$prevalence))

prevalence = list("Hamburg" = prevalencehb, "Bremen" = prevalencebr, "Berlin" = prevalencebe)

# Load income data for Germany
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

br = dt %>%
  filter(l11101 == 4)

be = dt %>%
  filter(l11101 == 11)

wagehb = hb$dailyinc
wagebr = br$dailyinc
wagebe = be$dailyinc

wages = list("Hamburg" = wagehb, "Bremen" = wagebr, "Berlin" = wagebe)

# # Plot density and histogram
# 
# ggplot(hb, aes(x=dailyinc)) + 
#   geom_histogram(aes(y=after_stat(density)), fill="white", color="black", bins=60) +  
#   geom_density(alpha=0.2, fill="#FF6666") +  
#   labs(title = "Histogram and kernel density of incomes in Hamburg",
#        x = "Daily incomes",
#        y = "Density") +
#   theme_bw() 


# Calculate the cost in parallel
plan(multisession)

cities_data <- list(
  Hamburg = list(wages = wages$Hamburg, prevalence = prevalence$Hamburg),
  Bremen = list(wages = wages$Bremen, prevalence = prevalence$Bremen),
  Berlin = list(wages = wages$Berlin, prevalence = prevalence$Berlin)
)

runsims = function(prevalence, wage_data) {
  future_map(prevalence, ~ econ(.x, n = 1000, cf = 10000, cv = 150, cl = 300, h = 0.5, tau0 = 750, data = wage_data, sims = 25), .options = furrr_options(seed = 300))
}


sims = future_map(names(cities_data), ~ {
  city_name <- .x
  city_data <- cities_data[[city_name]]
  prevalence_vector <- city_data$prevalence
  wage_data <- city_data$wages
  result_matrices = runsims(prevalence_vector, wage_data)
  
  # Flatten and combine the result matrices
  flat_result = do.call(bind_rows, lapply(result_matrices, as.data.frame))
  
  list(city = city_name, results = flat_result)
}, .options = furrr_options(seed = 300))

names(sims) <- names(cities_data)


# Function to calculate the lowest cost and corresponding algorithm for each prevalence level within each city
avelow = function(city_result) {
  city_result$results %>%
    group_by(p) %>%
    summarize(
      lowest_cost = min(MCosts, na.rm = TRUE),
      Algorithm = Algorithm[which.min(c(MCosts))]
    ) %>%
    ungroup()
}

minlow = function(city_result) {
  city_result$results %>%
    group_by(p) %>%
    summarize(
      lowest_cost = min(LCosts, na.rm = TRUE),
      Algorithm = Algorithm[which.min(c(LCosts))]
    ) %>%
    ungroup()
}

maxlow = function(city_result) {
  city_result$results %>%
    group_by(p) %>%
    summarize(
      lowest_cost = min(UCosts, na.rm = TRUE),
      Algorithm = Algorithm[which.min(c(UCosts))]
    ) %>%
    ungroup()
}


# Calculate the lowest cost and corresponding algorithm at each prevalence level for each city
meanlow = map(sims, avelow)
meanlow = bind_rows(meanlow, .id = "city")

lowecon = map(sims, minlow)
lowecon = bind_rows(lowecon, .id = "city")

maxecon = map(sims, maxlow)
maxecon = bind_rows(maxecon, .id = "city")

# Plot the results
x11()
algorithm_colors = c("One-stage" = "black",
                     "Two-stage" = "green",
                     "Three-stage" = "blue",
                     "Four-stage" = "red",
                     "Five-stage" = "yellow")
ggplot(meanlow, aes(x = p, y = lowest_cost, color = Algorithm)) +
  geom_line(aes(group = 1), linewidth = 1) +
  geom_line(data = lowecon, aes(y = lowest_cost, group = 1), linewidth = 0.5, alpha = 0.3) +
  geom_line(data = maxecon, aes(y = lowest_cost, group = 1), linewidth = 0.5, alpha = 0.3) +
  facet_wrap(~ city, nrow = 1, ncol = 3, scales = "free_y", 
             labeller = labeller(n = function(value) paste0("n = ", value))) +
  labs(title = "Progress of average economic cost per individual for the COVID-19 pandemic",
       x = "Prevalence",
       y = "Average economic cost per individual") +
  theme_bw() +
  ylim(60,175) +
  theme(legend.position = "right",
        legend.key.size = unit(3, "lines")) +
  scale_color_manual(values = algorithm_colors)







