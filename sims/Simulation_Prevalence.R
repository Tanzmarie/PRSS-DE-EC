# Dependencies
library(tidyverse)

source("functions/Tests.R")

prev = seq(0,0.35,0.01)

tests = lapply(X = prev, calculate_tests, n = 10000)

result_df <- do.call(rbind, lapply(seq_along(tests), function(i) {
  data.frame(
    Time = prev[i],
    Algorithm = tests[[i]][1],
    Tests = tests[[i]][, "Tests"]
  )
}))


ggplot(result_df, aes(x = Time, y = Tests, color = Algorithm)) +
  geom_line(aes(group = Algorithm), size = 1) +
  labs(title = "Evolution of tests for different prevalence values",
       x = "Prevalence",
       y = "Tests") +
  theme_minimal() +
  theme(legend.position = "right",
        legend.key.size = unit(3, "lines"))

