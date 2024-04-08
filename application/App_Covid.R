library(readr)
library(tidyverse)

source("functions/Tests.R")

COVID_19_Faelle_7_Tage_Inzidenz_Landkreise <- read_csv("application/COVID-19-Faelle_7-Tage-Inzidenz_Landkreise.csv")

data = COVID_19_Faelle_7_Tage_Inzidenz_Landkreise

# location: "09375" == Regensburg

location = unique(data$Landkreis_id)
test = data[which(data$Landkreis_id == "09375"),]
loc = "09375"


test$prevalence = ((test$`Inzidenz_7-Tage`/7) * 14)/100000

test$prevalence2 = test$`Faelle_7-Tage`/test$Bevoelkerung

ggplot(test, aes(x = Meldedatum, y = prevalence)) +
  geom_line() +
  labs(title = paste("Time Series Simulated Prevalence", paste("Regensburg", collapse = ", ")),
       x = "Date",
       y = "Prevalence") +
  theme_bw() 

tests = lapply(X = test$prevalence, calculate_tests, n = 1000)


# Convert the list of matrices to a data frame

result_df <- do.call(rbind, lapply(seq_along(tests), function(i) {
  data.frame(
    Time = i,
    Algorithm = tests[[i]][1],
    Tests = tests[[i]][, "Tests"],
    Negative_Deviation = tests[[i]][, "10% Negative"],
    Positive_Deviation = tests[[i]][, "10% Positive"]
  )
}))

# Plotting
ggplot(result_df, aes(x = Time, y = Tests, color = Algorithm)) +
  geom_line(aes(group = Algorithm), size = 1) +
  #geom_ribbon(aes(ymin = Negative_Deviation, ymax = Positive_Deviation, fill = Algorithm), alpha = 0.1) +
  labs(title = "Evolution of Tests Over Time",
       x = "Time",
       y = "Tests") +
  theme_minimal() +
  theme(legend.position = "right")