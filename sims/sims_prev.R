# Dependencies
source("functions/tests.R")

n = 1000
prev = seq(0,0.35,0.01)

set.seed(444)
tests = lapply(X = prev, calculate_tests, n = 1000, sims = 50)

result_df = do.call(rbind, lapply(seq_along(tests), function(i) {
  data.frame(
    Time = prev[i],
    Algorithm = tests[[i]][1],
    Theoretical = tests[[i]][, "Theoretical"],
    Tests = tests[[i]][, "Tests"],
    Lower = tests[[i]][, "Lower"],
    Upper = tests[[i]][, "Upper"]
  )
}))

algorithm_colors =  c("Individual" = "black",
                      "Dorfman" = "green",
                      "Double-Pooling" = "blue",
                      "R-Pooling" = "cyan2",
                      "3-Stage" = "red",
                      "4-Stage" = "darkgoldenrod"
)

ggplot(result_df, aes(x = Time, y = Tests/n, color = Algorithm)) +
  geom_line(aes(group = Algorithm), size = 1) +
  #geom_ribbon(aes(ymin = Lower/n, ymax = Upper/n, fill = Algorithm), alpha = 0.3) +  
  labs(title = "Progress of number of tests for different prevalence values",
       x = "Prevalence",
       y = "Tests per individual") +
  theme_bw() +
  theme(legend.position = "right",
        legend.key.size = unit(3, "lines"))+
  scale_color_manual(values = algorithm_colors)

