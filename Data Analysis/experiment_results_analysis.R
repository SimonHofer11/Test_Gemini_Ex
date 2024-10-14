#Skript zur Berechnung der Datenanalyse
getwd()

#1. Durchschnittswerte

# Benötigte Pakete laden
library(readr)
library(dplyr)
library(tidyr)

# Alle .RDS Dateien im Verzeichnis auflisten

#Monopoly 25 Rounds, base prompt
#x = "C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/gemini_results/Monopoly/25_Rounds_prompt_base" 

#Monopoly 25 rounds with help profit maximization
#x = "C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/gemini_results/Monopoly/25_Rounds_with_help_profit_max"

#Monopoly 25 Rounds Formula Profit Max
#x = "C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/gemini_results/Monopoly/25_Rounds_with_profit_max_formula"

#Monopoly 25 Rounds Formula Profit Max
#x = "C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/gemini_results/Monopoly/25_Rounds_temperature_0"

#Monopoly 25 Rounds PRO
#x = "C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/gemini_results/Monopoly/25_Rounds_PRO"




#Oligopoly Experiment 2 firms 25 runds without previous strategy
#x = "C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/gemini_results/Oligopoly/2_Firms_25_rounds/2_Firms_25_Rounds_wo_prev_strat"

# Oligopoly Experiment 2 firms 25 runs with previous strategy
#x = "C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/gemini_results/Oligopoly/2_Firms_25_rounds/2_Firms_25_Rounds_w_prev_strat"


#Oligopoly Experimen 2 Firms PRO Version
#x = "C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/gemini_results/Oligopoly/2_Firms_25_rounds/2_Firms_25_Rounds_w_prev_strat_gemini_pro"

#Oligopoly Experiment 2 Firms TEMPERATURE = 0
#x = "C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/gemini_results/Oligopoly/2_Firms_temperature_0"


#Oligopoly Experiment 2 Firms SELF CALCULATION
#x = "C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/gemini_results/Oligopoly/2_Firms_25_rounds/2_Firms_25_Rounds_self_calculation"

#Oligopol 3 Firmen 
#x = "C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/gemini_results/Oligopoly/3_Firms_25_rounds"


#Oligopol 3 Firmen PRO Version
#x = "C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/gemini_results/Oligopoly/3_Firms_25_rounds_PRO"


#Oligopoly 5 firms 25 rounds with prev strategy
#x = "C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/gemini_results/Oligopoly/5_Firms_25_rounds"

#Oligopoly 5 firms 25 rounds PRO
#x = "C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/gemini_results/Oligopoly/5_Firms_25_rounds_PRO"



#rds_files = list.files(path = "C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/gemini_results/Oligopoly/2_Firms_25_rounds/05092024_2_Firms_25_Rounds_w_prev_strat/oligopoly_2_firms_25_rounds_with_prev_strat_final.Rds")

rds_files <- list.files(path = paste0(x),pattern = "\\.Rds$", full.names = TRUE)

#saveRDS(experiment_results, file = "C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/gemini_results/Oligopoly/2_Firms_25_rounds/05092024_2_Firms_25_Rounds_w_prev_strat/oligopoly_2_firms_25_rounds_with_prev_strat_final.Rds")


# Eine leere Liste für die Ergebnisse erstellen
experiment_results <- list()

# Alle .RDS Dateien einlesen und in die Liste speichern mit benutzerdefinierten Namen
experiment_results <- lapply(seq_along(rds_files), function(i) {
  readRDS(rds_files[i])
})

#Datei vereinen:
#experiment_results[[1]]$game_25_rounds_run_3 = experiment_results[[2]]$game_25_rounds_run_1
#experiment_results[[1]]$game_5_player_run_4 = experiment_results[[2]]$game_5_player_run_2
#experiment_results[[1]]$game_5_player_run_5 = experiment_results[[3]]$game_5_player_run_1
#experiment_results[[1]]$game_5_player_run_6 = experiment_results[[4]]$game_5_player_run_1



#saveRDS(object = experiment_results[[1]], file = "C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/gemini_results/Monopoly/25_Rounds_PRO/monopoly_joined.Rds")

#experiment_results = readRDS("C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/gemini_results/Oligopoly/2_Firms_25_rounds/05092024_2_Firms_25_Rounds_w_prev_strat/oligopoly_2_firms_25_rounds_with_prev_strat_final.Rds")

#experiment_results[[1]][["game_25_rounds_run_1"]][["player_dfs"]][["q"]]
q_values <- numeric()

# Schleife über die Abläufe 1 bis 6
for (i in 1:6) {
  # Namen des aktuellen Ablaufs generieren
  #Mmonopoly:
  #ablauf_name <- paste0("game_25_rounds_run_", i)
  #Oligopoly with prev strat:
  #2 Player
  ablauf_name <- paste0("game_2_player_run_", i)
  
  #3 Player
  #ablauf_name <- paste0("game_3_player_run_", i)
  
  #5 Player
  #ablauf_name <- paste0("game_5_player_run_", i)
  
  
  # Extrahiere die Werte von "Q" und füge sie zur Liste hinzu
  #Monopoly:
  #q_values <- c(q_values, experiment_results[[1]][[ablauf_name]][["player_dfs"]][["q"]])
  
  #Oligopoly without prev strat
  #q_values = c(q_values, experiment_results[[i]][["player_dfs"]][[1]][["Q"]])
  
  #oligopoly with prev strat:
  #q_values <- c(q_values, experiment_results[[1]][[1]][[ablauf_name]][["player_dfs"]][[1]][["Q"]])
  
  #Oligopoly pro version
  q_values <- c(q_values, experiment_results[[1]][[ablauf_name]][["player_dfs"]][[1]][["Q"]])
  
}

# Durchschnitt der Q-Werte berechnen
q_mean <- mean(q_values, na.rm = TRUE)

#Oligopoly:
#2 Firms
r = q_mean / 66
#3 Firms
#r = q_mean/74.25
#5firms
#r = q_mean/82.5

#Monopoly:
#r = q_mean/49.5
#r

q_values_17_25 <- numeric()

# Schleife über die Abläufe 1 bis 6
for (i in 1:6) {
  # Namen des aktuellen Ablaufs generieren
  #Mmonopoly:
  ablauf_name <- paste0("game_25_rounds_run_", i)
  #Oligopoly:
  #2 Firms
  #ablauf_name <- paste0("game_2_player_run_", i)
  
  #3 Firms
  #ablauf_name <- paste0("game_3_player_run_", i)
  
  #5 Firms
  #ablauf_name <- paste0("game_5_player_run_", i)
  
  
  # Extrahiere die Werte von "Q" und füge sie zur Liste hinzu
  #Monopoly:
  q_values_17_25 <- c(q_values_17_25, experiment_results[[1]][[ablauf_name]][["player_dfs"]][["q"]][17:25])
  
  #Oligopoly without prev strat:
  #Oligopoly without prev strat
  #q_values_17_25 = c(q_values_17_25, experiment_results[[i]][["player_dfs"]][[1]][["Q"]][17:25])
  
  #oligopoly with prev strat:
  #q_values_17_25 <- c(q_values_17_25, experiment_results[[1]][[1]][[ablauf_name]][["player_dfs"]][[1]][["Q"]][17:25])
  #q_values_17_25 <- c(q_values_17_25, experiment_results[[1]][[ablauf_name]][["player_dfs"]][[1]][["Q"]][17:25])
  #Oligopoly pro version
  #q_values_17_25 <- c(q_values_17_25, experiment_results[[1]][[ablauf_name]][["player_dfs"]][[1]][["Q"]][17:25])
  

}

# Durchschnitt der Q-Werte berechnen

q_mean_17_25 <- mean(q_values_17_25, na.rm = TRUE)
q_mean_17_25
#Monopoly
r_17_25 = q_mean_17_25/49.5


#Oligopoly
#2 Firms
#r_17_25 = q_mean_17_25/66
#3 Firms
#r_17_25 = q_mean_17_25/74.25

#5firms
#r_17_25 = q_mean_17_25/82.5



#2. Bar plot

#Monopoly
q_nash = 49.5
#Oligopoly
#2 Firmen
#q_nash = 66

#3Firmen
#q_nash = 74.5

#5Firmen
#q_nash = 82.5
library(ggplot2)



# Datenrahmen erstellen
data <- data.frame(
  x = factor(2),  # X-Achse auf 2 fixiert und als Faktor definiert
  value = c(q_nash, q_mean, q_mean_17_25),
  variable = factor(c("Nash", "Mean 1-25", "Mean 17-25"))
)

# Barplot erstellen
plot = ggplot(data, aes(x = x, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  scale_fill_manual(values = c("Nash" = "red", "Mean 1-25" = "blue", "Mean 17-25" = "green")) +
  labs(x = "Number of Firms", y = "Q-Werte", fill = "Legende") +
  theme_minimal()


cbind(q_nash,q_mean,r,q_mean_17_25,r_17_25)
df =as.data.frame(cbind(q_nash,q_mean,r,q_mean_17_25,r_17_25))
plot

name = "Monopoly PRO"
df$name = name


list = list(name = name, values = df, plot = plot)


saveRDS(object = list, file = "C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/gemini_results/Final_Results/Monopoly_PRO.Rds")
#x = readRDS("C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/gemini_results/Final_Results/2_Firms_25_Rounds_w_prev_strategy.Rds")
#x$plot






























