### Clear workspace and load required libraries
rm(list = ls())
gc()
options(scipen = 900)

pacman::p_load(tidyverse, readr)


### (1) Prevalence values 
data = read_csv("application/data/COVID-19-Faelle_7-Tage-Inzidenz_Landkreise.csv")

prevalences = data %>%
  filter(Landkreis_id %in% c("02000", "04011", "11001")) %>%
  mutate(
    prevalence = ((`Inzidenz_7-Tage` / 7) * 14) / 100000,
    region = case_when(
      Landkreis_id == "02000" ~ "Hamburg",
      Landkreis_id == "04011" ~ "Bremen",
      Landkreis_id == "11001" ~ "Berlin-Mitte"
    )
  )

ggplot(prevalences, aes(x = Meldedatum, y = prevalence)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ region, ncol = 1, scales = "free_y") +
  labs(
    x = "Date",
    y = "Prevalence",
  ) +
  theme_bw(base_size = 15) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )

### (2) Income values 
pgen = read_csv("D:/PhD/EconEvalGT/Data/cs-transfer/SOEP-CORE.v38.1_eu_CSV/CSV/soepdata/pgen.csv")
pequiv = read_csv("D:/PhD/EconEvalGT/Data/cs-transfer/SOEP-CORE.v38.1_eu_CSV/CSV/soepdata/pequiv.csv")

# Data preparation
inc = pgen %>%
  filter(pglabgro > 0 & pgtatzeit > 0 & syear %in% seq(2019, 2021, 1)) %>%
  group_by(pid) %>%
  filter(syear == max(syear)) %>%
  ungroup()

loc = pequiv %>%
  filter(l11101 > 0 & syear %in% seq(2019, 2021, 1) & pid %in% inc$pid) %>%
  group_by(pid) %>%
  filter(syear == max(syear)) %>%
  ungroup()

# Join datasets
dt = inner_join(inc, loc[, c("pid", "l11101")], by = "pid")

# Calculate daily income
dt = dt %>%
  mutate(dailyinc = (pglabgro / (pgtatzeit / 5)) / 4.345,
         region = case_when(
           l11101 == 2  ~ "Hamburg",
           l11101 == 4  ~ "Bremen",
           l11101 == 11 ~ "Berlin",
           TRUE         ~ NA_character_
         )) %>%
  filter(!is.na(region))

# Faceted histogram + density
ggplot(dt, aes(x = dailyinc)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill = "white", color = "black", bins = 200) +
  geom_density(alpha = 0.2, fill = "#FF6666") +
  facet_wrap(~ region, ncol = 1, scales = "free_y") +
  labs(
    x = "Daily income",
    y = "Density"
  ) +
  coord_cartesian(xlim = c(0, 1000)) +   # cut axis at 1000
  theme_bw(base_size = 15)  +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )


