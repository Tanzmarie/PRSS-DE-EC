# Dependencies
source("functions/tests.R")

prev = seq(0,1,0.01)

tests = lapply(X = prev, calculate_tests, n = 1000, sims = 0)

result_df <- do.call(rbind, lapply(seq_along(tests), function(i) {
  data.frame(
    Time = prev[i],
    Algorithm = tests[[i]][1],
    Theoretical = tests[[i]][, "Theoretical"],
    Tests = tests[[i]][, "Tests"],
    Lower = tests[[i]][, "Lower"],
    Upper = tests[[i]][, "Upper"]
  )
}))


ggplot(result_df, aes(x = Time, y = Theoretical, color = Algorithm)) +
  geom_line(aes(group = Algorithm), size = 1) +
  # geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = Algorithm), alpha = 0.3) +  # Mapping fill to Algorithm
  labs(title = "Evolution of tests for different prevalence values",
       x = "Prevalence",
       y = "Tests") +
  theme_minimal() +
  theme(legend.position = "right",
        legend.key.size = unit(3, "lines"))

