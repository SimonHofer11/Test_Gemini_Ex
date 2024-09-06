#Skript zur Berechnung der Datenanalyse


#1. Durchschnittswerte

# Benötigte Pakete laden
library(readr)

# Alle .RDS Dateien im Verzeichnis auflisten

#Experiment 2 firms 25 runs:
#"C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/gemini_results/Oligopoly/2_Firms_25_rounds/05092024_2_Firms_25_Rounds_w_prev_strat/oligopoly_2_firms_25_rounds_with_prev_strat_final.Rds"


rds_files <- list.files(path = "C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/gemini_results/Oligopoly/2_Firms_25_rounds/05092024_2_Firms_25_Rounds_w_prev_strat", pattern = "\\.Rds$", full.names = TRUE)

saveRDS(experiment_results, file = "C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/gemini_results/Oligopoly/2_Firms_25_rounds/05092024_2_Firms_25_Rounds_w_prev_strat/oligopoly_2_firms_25_rounds_with_prev_strat_final.Rds")


# Eine leere Liste für die Ergebnisse erstellen
experiment_results <- list()

# Alle .RDS Dateien einlesen und in die Liste speichern mit benutzerdefinierten Namen
experiment_results <- lapply(seq_along(rds_files), function(i) {
  readRDS(rds_files[i])
})



q_values <- numeric()

# Schleife über die Abläufe 1 bis 6
for (i in 1:6) {
  # Namen des aktuellen Ablaufs generieren
  ablauf_name <- paste0("game_2_player_run_", i)
  
  # Extrahiere die Werte von "Q" und füge sie zur Liste hinzu
  q_values <- c(q_values, experiment_results[[1]][[ablauf_name]][["player_dfs"]][[1]][["Q"]])
}

# Durchschnitt der Q-Werte berechnen
q_mean <- mean(q_values, na.rm = TRUE)

r = q_mean/66
r

q_values_17_25 <- numeric()

# Schleife über die Abläufe 1 bis 6
for (i in 1:6) {
  # Namen des aktuellen Ablaufs generieren
  ablauf_name <- paste0("game_2_player_run_", i)
  
  # Extrahiere die Werte von "Q" und füge sie zur Liste hinzu
  q_values_17_25 <- c(q_values_17_25, experiment_results[[1]][[ablauf_name]][["player_dfs"]][[1]][["Q"]][17:25])
}

# Durchschnitt der Q-Werte berechnen
q_mean_17_25 <- mean(q_values_17_25, na.rm = TRUE)
q_mean_17_25

r_17_25 = q_mean_17_25/66



#2. Bar plot

q_nash = 66

library(ggplot2)



# Datenrahmen erstellen
data <- data.frame(
  x = factor(2),  # X-Achse auf 2 fixiert und als Faktor definiert
  value = c(q_nash, q_mean, q_mean_17_25),
  variable = factor(c("Nash", "Mean 1-25", "Mean 17-25"))
)

# Barplot erstellen
ggplot(data, aes(x = x, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  scale_fill_manual(values = c("Nash" = "red", "Mean 1-25" = "blue", "Mean 17-25" = "green")) +
  labs(x = "Number of Firms", y = "Q-Werte", fill = "Legende") +
  theme_minimal()


cbind(q_nash,q_mean,r,q_mean_17_25,r_17_25)
