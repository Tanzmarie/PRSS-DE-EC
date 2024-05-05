library(readr)
library(tidyverse)

source("functions/tests.R")

# Hamburg = "02000"
data <- read_csv("application/data/COVID-19-Faelle_7-Tage-Inzidenz_Landkreise.csv")
hamburg = data[which(data$Landkreis_id == "02000"),]


# Calculate prevalence values 
hamburg$prevalence = ((hamburg$`Inzidenz_7-Tage`/7) * 14)/100000

ggplot(hamburg, aes(x = Meldedatum, y = prevalence)) +
  geom_line() +
  labs(title = paste("Progress of prevalence values in the COVID-19 pandemic in Hamburg"),
       x = "Date",
       y = "Prevalence") +
  theme_bw() 

summary(hamburg$prevalence)

# Calculate Tests
set.seed(444)
tests = lapply(X = hamburg$prevalence, calculate_tests, n = 1000, sims = 10)

# Convert the list of matrices to a data frame

result_df <- do.call(rbind, lapply(seq_along(tests), function(i) {
  data.frame(
    Time = i,
    Algorithm = tests[[i]][1],
    Theoretical = tests[[i]][,"Theoretical"],
    Tests = tests[[i]][, "Tests"],
    Lower = tests[[i]][, "Lower"],
    Upper = tests[[i]][, "Upper"]
  )
}))

# Plotting
ggplot(result_df, aes(x = Time, y = Tests, color = Algorithm)) +
  geom_line(aes(group = Algorithm), linewidth = 1) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = Algorithm), alpha = 0.1) +
  labs(title = "Progress of tests over time horizon of COVID-19 pandemic in hamburg",
       x = "Time in days",
       y = "Tests") +
  theme_minimal() +
  theme(legend.position = "right")
