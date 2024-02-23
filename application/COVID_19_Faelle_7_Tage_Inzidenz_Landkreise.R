library(readr)
library(ggplot2)

source("functions/DynamPrev.R")

COVID_19_Faelle_7_Tage_Inzidenz_Landkreise <- read_csv("application/COVID-19-Faelle_7-Tage-Inzidenz_Landkreise.csv")

data = COVID_19_Faelle_7_Tage_Inzidenz_Landkreise


# location: "09375" == Regensburg

location = unique(data$Landkreis_id)
test = data[which(data$Landkreis_id == location[264]),]
loc = location[264]


test$prevalence = ((test$`Inzidenz_7-Tage`/7) * 14)/100000

test$prevalence2 = test$`Faelle_7-Tage`/test$Bevoelkerung

ggplot(test, aes(x = Meldedatum, y = prevalence)) +
  geom_line() +
  labs(title = paste("Time Series of Covid-19 Prevalence at Location", paste(loc, collapse = ", ")),
       x = "Date",
       y = "Prevalence") +
  theme_bw() 


