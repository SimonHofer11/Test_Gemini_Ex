setwd("C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/gemini_results/Final_Results")
# Alle Dateinamen im aktuellen Verzeichnis abrufen
dateien <- list.files()

# Optional: Nur Dateien mit einer bestimmten Erweiterung auswählen (z.B. .csv)
# dateien <- list.files(pattern = "\\.csv$")

# Alle Dateien einlesen und in einer Liste speichern
daten_liste <- lapply(dateien, function(datei) {
  # Datei einlesen (passe die Funktion an deinen Dateityp an)
  readRDS(datei)
})
daten_liste[[1]]$values$q_i_nash= 33
daten_liste[[1]]$values$n= 2
daten_liste[[1]]$values$p_i_nash= 1089
daten_liste[[1]]$values$collusive_Q = 49.5
daten_liste[[1]]$values$collusvie_pi=1225.13
daten_liste[[1]]$values$pi_avg_experiment = daten_liste[[1]]$values$q_mean * (100-daten_liste[[1]]$values$q_mean)/ daten_liste[[1]]$values$n - daten_liste[[1]]$values$q_mean

daten_liste[[2]]$values$q_i_nash= 33
daten_liste[[2]]$values$n= 2
daten_liste[[2]]$values$p_i_nash= 1089
daten_liste[[2]]$values$collusive_Q = 49.5
daten_liste[[2]]$values$collusvie_pi=1225.13
daten_liste[[2]]$values$pi_avg_experiment = daten_liste[[2]]$values$q_mean * (100-daten_liste[[2]]$values$q_mean)/ daten_liste[[2]]$values$n - daten_liste[[2]]$values$q_mean

daten_liste[[3]]$values$q_i_nash= 33
daten_liste[[3]]$values$n= 2
daten_liste[[3]]$values$p_i_nash= 1089
daten_liste[[3]]$values$collusive_Q = 49.5
daten_liste[[3]]$values$collusvie_pi=1225.13
daten_liste[[3]]$values$pi_avg_experiment = daten_liste[[3]]$values$q_mean * (100-daten_liste[[3]]$values$q_mean)/ daten_liste[[3]]$values$n - daten_liste[[3]]$values$q_mean

daten_liste[[4]]$values$q_i_nash= 33
daten_liste[[4]]$values$n= 2
daten_liste[[4]]$values$p_i_nash= 1089
daten_liste[[4]]$values$collusive_Q = 49.5
daten_liste[[4]]$values$collusvie_pi=1225.13
daten_liste[[4]]$values$pi_avg_experiment = daten_liste[[4]]$values$q_mean * (100-daten_liste[[4]]$values$q_mean)/ daten_liste[[4]]$values$n - daten_liste[[4]]$values$q_mean

daten_liste[[5]]$values$q_i_nash= 33
daten_liste[[5]]$values$n= 2
daten_liste[[5]]$values$p_i_nash= 1089
daten_liste[[5]]$values$collusive_Q = 49.5
daten_liste[[5]]$values$collusvie_pi=1225.13
daten_liste[[5]]$values$pi_avg_experiment = daten_liste[[5]]$values$q_mean * (100-daten_liste[[5]]$values$q_mean)/ daten_liste[[5]]$values$n - daten_liste[[5]]$values$q_mean


daten_liste[[6]]$values$q_i_nash= 24.75
daten_liste[[6]]$values$n= 3
daten_liste[[6]]$values$p_i_nash= 612.56
daten_liste[[6]]$values$collusive_Q = 49.5
daten_liste[[6]]$values$collusvie_pi=816.75
daten_liste[[6]]$values$pi_avg_experiment = daten_liste[[6]]$values$q_mean * (100-daten_liste[[6]]$values$q_mean)/ daten_liste[[6]]$values$n - daten_liste[[6]]$values$q_mean


daten_liste[[7]]$values$q_i_nash= 24.75
daten_liste[[7]]$values$n= 3
daten_liste[[7]]$values$p_i_nash= 612.56
daten_liste[[7]]$values$collusive_Q = 49.5
daten_liste[[7]]$values$collusvie_pi=816.75
daten_liste[[7]]$values$pi_avg_experiment = daten_liste[[7]]$values$q_mean * (100-daten_liste[[7]]$values$q_mean)/ daten_liste[[7]]$values$n - daten_liste[[7]]$values$q_mean


daten_liste[[8]]$values$q_i_nash= 16.5
daten_liste[[8]]$values$n= 5
daten_liste[[8]]$values$p_i_nash= 272.25
daten_liste[[8]]$values$collusive_Q = 49.5
daten_liste[[8]]$values$collusvie_pi=490.05
daten_liste[[8]]$values$pi_avg_experiment = daten_liste[[8]]$values$q_mean * (100-daten_liste[[8]]$values$q_mean)/ daten_liste[[8]]$values$n - daten_liste[[8]]$values$q_mean


daten_liste[[9]]$values$q_i_nash= 16.5
daten_liste[[9]]$values$n= 5
daten_liste[[9]]$values$p_i_nash= 272.25
daten_liste[[9]]$values$collusive_Q = 49.5
daten_liste[[9]]$values$collusvie_pi=490.05
daten_liste[[9]]$values$pi_avg_experiment = daten_liste[[9]]$values$q_mean * (100-daten_liste[[9]]$values$q_mean)/ daten_liste[[9]]$values$n - daten_liste[[9]]$values$q_mean

daten_liste[[10]]$values$pi_max = 2450.25
daten_liste[[10]]$values$pi_avg_experiment = daten_liste[[10]]$values$q_mean * (100-daten_liste[[10]]$values$q_mean) - daten_liste[[10]]$values$q_mean


daten_liste[[11]]$values$pi_max = 2450.25
daten_liste[[11]]$values$pi_avg_experiment = daten_liste[[11]]$values$q_mean * (100-daten_liste[[11]]$values$q_mean) - daten_liste[[11]]$values$q_mean


daten_liste[[12]]$values$pi_max = 2450.25
daten_liste[[12]]$values$pi_avg_experiment = daten_liste[[12]]$values$q_mean * (100-daten_liste[[12]]$values$q_mean) - daten_liste[[12]]$values$q_mean


daten_liste[[13]]$values$pi_max = 2450.25
daten_liste[[13]]$values$pi_avg_experiment = daten_liste[[13]]$values$q_mean * (100-daten_liste[[13]]$values$q_mean) - daten_liste[[13]]$values$q_mean

daten_liste[[14]]$values$pi_max = 2450.25
daten_liste[[14]]$values$pi_avg_experiment = daten_liste[[14]]$values$q_mean * (100-daten_liste[[14]]$values$q_mean) - daten_liste[[14]]$values$q_mean
 

# Nur die ersten 9 Listenobjekte auswählen
daten_liste_auswahl <- daten_liste[1:9]

# Extrahiere die 'values' aus den ausgewählten Listenobjekten und wandle sie in Dataframes um
daten_frames <- lapply(daten_liste_auswahl, function(x) {
  as.data.frame(x$values)
})

# Kombiniere alle Dataframes zu einem großen Dataframe
kombinierter_df <- do.call(rbind, daten_frames)
library(dplyr)

kombinierter_df <- kombinierter_df %>%
  relocate(name, .before = everything()) %>% # 'name' als erste Spalte
  rename(Q_nash = q_nash, Q_mean = q_mean, Q_mean_17_25 = q_mean_17_25) 
kombinierter_df <- kombinierter_df %>% 
  rename(qi_nash = q_i_nash, pi_nash = p_i_nash, pi_collusive = collusvie_pi)
# Spalten umbenennen
kombinierter_df$n = c(2,2,2,2,2,3,3,5,5)
kombinierter_df = kombinierter_df %>% 
  mutate(pi_avg_experiment = (Q_mean/n) * (100-Q_mean) - Q_mean)
      
df_oligopol = kombinierter_df %>% 
  mutate(Index = 1:nrow(df_oligopol))






###########MONOPOL
# Nur die ersten 9 Listenobjekte auswählen
daten_liste_auswahl_monopol <- daten_liste[10:13]

# Extrahiere die 'values' aus den ausgewählten Listenobjekten und wandle sie in Dataframes um
daten_frames <- lapply(daten_liste_auswahl_monopol, function(x) {
  as.data.frame(x$values)
})

# Kombiniere alle Dataframes zu einem großen Dataframe
kombinierter_df_monopol <- do.call(rbind, daten_frames)


kombinierter_df_monopol <- kombinierter_df_monopol %>%
  relocate(name, .before = everything()) %>% # 'name' als erste Spalte
  rename(Q_nash = q_nash, Q_mean = q_mean, Q_mean_17_25 = q_mean_17_25) 
df_monopol = kombinierter_df_monopol



saveRDS(df_oligopol, file = "C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/gemini_results/Final_Results/df_oligopol.Rds")
saveRDS(df_monopol, file = "C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/gemini_results/Final_Results/df_monopol.Rds")







#2 Firms


df_oligopol_2_firms <- df_oligopol %>% 
  filter(n==2)


#Quantity 

# Die Beschriftungen der x-Achse und Legende festlegen
x_labels <- c("Pro Version Gemini", 
              "No Calculation support", 
              "With information about previous strategy", 
              "Without information about previous strategy", 
              "Temperature set from 1 to 0")

# Erstellung des Barplots
ggplot(df_oligopol_2_firms, aes(x = factor(Index, levels = 1:5, labels = x_labels), y = Q_mean, fill = factor(Index))) +
  geom_bar(stat = "identity") +  # Barplot mit verschiedenen Farben
  geom_hline(yintercept = 49.5, color = "red", linetype = 1, size = 2) + # Horizontale Linie bei 49.5
  geom_hline(yintercept = 66, color = "blue", linetype = 1, size = 2) +  # Horizontale Linie bei 66
  labs(
    x = "Variation",  # X-Achse benennen
    y = "Average Quantity",  # Y-Achse benennen
    title = "Experimental Results 2 Firms with 25 Rounds",  # Titel des Plots
    subtitle = "Average aggregated market quantities after 6 runs of the experiment",  # Hinweis unter dem Titel
    fill = "Legend"  # Titel der Legende für die Balken
  ) +
  scale_y_continuous(breaks =c(0,10,20,30,40,49.5,60,66,70,80)) +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854"), labels = x_labels) + # Farben und Legenden-Labels definieren
  theme_minimal() +  # Minimaler Stil für den Plot
  theme(
    
    #axis.text.x = element_text(angle = 45, hjust = 1),  # Beschriftungen der x-Achse neigen, damit sie besser lesbar sind
    axis.text.x = element_blank(),
    plot.title = element_text(size = 16, face = "bold"),  # Titel stilistisch hervorheben
    plot.subtitle = element_text(size = 12, face = "italic"),  # Untertitel stilistisch hervorheben
    #legend.position = "top"  # Legende oben anzeigen
  ) +
  annotate("text", x = 5, y = 49.5, label = "Quantity Collusion", color = "red", vjust = -1, hjust = 1) +
  annotate("text", x = 5, y = 66, label = "Quantity Nash", color = "blue", vjust = -1, hjust = 1)



#PI

# Die Beschriftungen der x-Achse und Legende festlegen
x_labels <- c("Pro Version Gemini", 
              "No Calculation support", 
              "With information about previous strategy", 
              "Without information about previous strategy", 
              "Temperature set from 1 to 0")

# Erstellung des Barplots
ggplot(df_oligopol_2_firms, aes(x = factor(Index, levels = 1:5, labels = x_labels), y = pi_avg_experiment, fill = factor(Index))) +
  geom_bar(stat = "identity") +  # Barplot mit verschiedenen Farben
  geom_hline(yintercept = 1225.13, color = "red", linetype = 1, size = 2) + # Horizontale Linie bei 49.5
  #geom_hline(yintercept = 66, color = "blue", linetype = 1, size = 2) +  # Horizontale Linie bei 66
  labs(
    x = "Variation",  # X-Achse benennen
    y = "Average Profit",  # Y-Achse benennen
    title = "Experimental Results 2 Firms with 25 Rounds",  # Titel des Plots
    subtitle = "Average company profits after 6 runs of the experiment",  # Hinweis unter dem Titel
    fill = "Legend"  # Titel der Legende für die Balken
  ) +
  scale_y_continuous(breaks = c(0,500,800,1000,1300), limits = c(0,1300)) +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854"), labels = x_labels) + # Farben und Legenden-Labels definieren

  theme_minimal() +  # Minimaler Stil für den Plot
  theme(
    
    #axis.text.x = element_text(angle = 45, hjust = 1),  # Beschriftungen der x-Achse neigen, damit sie besser lesbar sind
    axis.text.x = element_blank(),
    plot.title = element_text(size = 16, face = "bold"),  # Titel stilistisch hervorheben
    plot.subtitle = element_text(size = 12, face = "italic"),  # Untertitel stilistisch hervorheben
    #legend.position = "top"  # Legende oben anzeigen
  ) +
 # annotate("text", x = 5, y = 49.5, label = "Quantity Collusion", color = "red", vjust = -1, hjust = 1) +
  annotate("text", x = 5, y = 1225.13, label = "Collusion Profit", color = "red", vjust = -1, hjust = 1)









#3 Firms


df_oligopol_3_firms <- df_oligopol %>% 
  filter(n==3)


#Quantity 

# Die Beschriftungen der x-Achse und Legende festlegen
x_labels <- c("Normal Version", "Pro Version")

# Erstellung des Barplots
ggplot(df_oligopol_3_firms, aes(x = factor(Index, levels = 6:7, labels = x_labels), y = Q_mean, fill = factor(Index))) +
  geom_bar(stat = "identity") +  # Barplot mit verschiedenen Farben
  geom_hline(yintercept = 49.5, color = "black", linetype = 1, size = 2) + # Horizontale Linie bei 49.5
  geom_hline(yintercept = 74.5, color = "blue", linetype = 1, size = 2) +  # Horizontale Linie bei 66
  labs(
    x = "Variation",  # X-Achse benennen
    y = "Average Quantity",  # Y-Achse benennen
    title = "Experimental Results 3 Firms with 25 Rounds",  # Titel des Plots
    subtitle = "Average aggregated market quantities after 6 runs of the experiment",  # Hinweis unter dem Titel
    fill = "Legend"  # Titel der Legende für die Balken
  ) +
  scale_y_continuous(breaks =c(0,10,40,49.5,60,70,74.5,80), limits = c(0,80)) +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854"), labels = x_labels) + # Farben und Legenden-Labels definieren
  theme_minimal() +  # Minimaler Stil für den Plot
  theme(
    
    #axis.text.x = element_text(angle = 45, hjust = 1),  # Beschriftungen der x-Achse neigen, damit sie besser lesbar sind
    axis.text.x = element_blank(),
    plot.title = element_text(size = 16, face = "bold"),  # Titel stilistisch hervorheben
    plot.subtitle = element_text(size = 12, face = "italic"),  # Untertitel stilistisch hervorheben
    #legend.position = "top"  # Legende oben anzeigen
  ) +
  annotate("text", x = 2, y = 49.5, label = "Quantity Collusion", color = "black", vjust = -1, hjust = 1) +
  annotate("text", x = 2, y = 74.5, label = "Quantity Nash", color = "blue", vjust = -1, hjust = 1)






#5 Firms


df_oligopol_5_firms <- df_oligopol %>% 
  filter(n==5)


#Quantity 

# Die Beschriftungen der x-Achse und Legende festlegen
x_labels <- c("Normal Version", "Pro Version")

# Erstellung des Barplots
ggplot(df_oligopol_5_firms, aes(x = factor(Index, levels = 8:9, labels = x_labels), y = Q_mean, fill = factor(Index))) +
  geom_bar(stat = "identity") +  # Barplot mit verschiedenen Farben
  geom_hline(yintercept = 49.5, color = "black", linetype = 1, size = 2) + # Horizontale Linie bei 49.5
  geom_hline(yintercept = 82.5, color = "blue", linetype = 1, size = 2) +  # Horizontale Linie bei 66
  labs(
    x = "Variation",  # X-Achse benennen
    y = "Average Quantity",  # Y-Achse benennen
    title = "Experimental Results 5 Firms with 25 Rounds",  # Titel des Plots
    subtitle = "Average aggregated market quantities after 6 runs of the experiment",  # Hinweis unter dem Titel
    fill = "Legend"  # Titel der Legende für die Balken
  ) +
  scale_y_continuous(breaks =c(0,10,40,49.5,60,70,82.5,90), limits = c(0,90)) +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854"), labels = x_labels) + # Farben und Legenden-Labels definieren
  theme_minimal() +  # Minimaler Stil für den Plot
  theme(
    
    #axis.text.x = element_text(angle = 45, hjust = 1),  # Beschriftungen der x-Achse neigen, damit sie besser lesbar sind
    axis.text.x = element_blank(),
    plot.title = element_text(size = 16, face = "bold"),  # Titel stilistisch hervorheben
    plot.subtitle = element_text(size = 12, face = "italic"),  # Untertitel stilistisch hervorheben
    #legend.position = "top"  # Legende oben anzeigen
  ) +
  annotate("text", x = 2, y = 49.5, label = "Quantity Collusion", color = "black", vjust = -1, hjust = 1) +
  annotate("text", x = 2, y = 82.5, label = "Quantity Nash", color = "blue", vjust = -1, hjust = 1)
















#Monopol
df_monopol <- df_monopol %>% 
  mutate(Index = 1:4)
# Die Beschriftungen der x-Achse und Legende festlegen
x_labels <- c("Base Prompt","Direct Profit Maximum Formula","Help to find Profit Maximum","Temperature 0")

# Erstellung des Barplots
ggplot(df_monopol, aes(x = factor(Index, levels = 1:4, labels = x_labels), y = Q_mean, fill = factor(Index))) +
  geom_bar(stat = "identity") +  # Barplot mit verschiedenen Farben
  geom_hline(yintercept = 49.5, color = "red", linetype = 1, size = 2) + # Horizontale Linie bei 49.5
  #geom_hline(yintercept = 66, color = "blue", linetype = 1, size = 2) +  # Horizontale Linie bei 66
  labs(
    x = "Variation",  # X-Achse benennen
    y = "Average Quantity",  # Y-Achse benennen
    title = "Experimental Results Monopoly 25 Rounds",  # Titel des Plots
    subtitle = "Average aggregated market quantities after 6 runs of the experiment",  # Hinweis unter dem Titel
    fill = "Legend"  # Titel der Legende für die Balken
  ) +
  scale_y_continuous(breaks =c(0,10,20,30,40,49.5,55),limits = c(0,55)) +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854"), labels = x_labels) + # Farben und Legenden-Labels definieren
  theme_minimal() +  # Minimaler Stil für den Plot
  theme(
    
    #axis.text.x = element_text(angle = 45, hjust = 1),  # Beschriftungen der x-Achse neigen, damit sie besser lesbar sind
    axis.text.x = element_blank(),
    plot.title = element_text(size = 16, face = "bold"),  # Titel stilistisch hervorheben
    plot.subtitle = element_text(size = 12, face = "italic"),  # Untertitel stilistisch hervorheben
    #legend.position = "top"  # Legende oben anzeigen
  ) +
  annotate("text", x = 5, y = 49.5, label = "Quantity Collusion", color = "red", vjust = -1, hjust = 1) 
  #annotate("text", x = 5, y = 66, label = "Quantity Nash", color = "blue", vjust = -1, hjust = 1)









#####Pro vs Normal

df_oligopol <- df_oligopol %>% 
  mutate(is_pro = c(1,0,0,0,0,0,1,0,1),
         is_pro_same_setting = c(1,NA,0,NA,NA,0,1,0,1))


df_oligopol <- df_oligopol %>% 
  mutate(pi_exp_to_pi_collusion_ratio = pi_avg_experiment/pi_collusive,
         pi_exp_to_pi_nash_ratio = pi_avg_experiment/pi_nash)


df_oligopol <- df_oligopol %>% 
  mutate(Q_exp_to_pi_collusion_ratio = Q_mean/collusive_Q,
         Q_exp_to_pi_nash_ratio = Q_mean/Q_nash)




##Profit to Collusion Ratio
df_oligopol_same_setting = df_oligopol %>% 
  filter(!is.na(is_pro_same_setting))

library(ggplot2)

ggplot(df_oligopol_same_setting, aes(x = factor(is_pro, labels = c("Normal Version (Gemini Flash 1.5)", "Pro Version (Gemini Pro 1.5)")), y = pi_exp_to_pi_collusion_ratio)) +
  geom_boxplot(fill = c("#66c2a5", "#fc8d62")) +
  labs(
    x = "Group",
    y = "Average Profit in Experiment to Collusion Profit Ratio ",
    title = "Ratio of Profit Results of the Experiments in Comparison to Collusion Profits by Version"
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0.3, 1), labels = scales::percent)




###Profit to Nash
library(ggplot2)

ggplot(df_oligopol_same_setting, aes(x = factor(is_pro, labels = c("Normal Version (Gemini Flash 1.5)", "Pro Version (Gemini Pro 1.5)")), y = pi_exp_to_pi_nash_ratio)) +
  geom_boxplot(fill = c("#66c2a5", "#fc8d62")) +
  labs(
    x = "Group",
    y = "Average Profit in Experiment to Nash Profit Ratio ",
    title = "Ratio of Profit Results of the Experiments in Comparison to Nash Profits by Version"
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0.6, 1.3), labels = scales::percent)


###Profit to Nash
library(ggplot2)

ggplot(df_oligopol_same_setting, aes(x = factor(is_pro, labels = c("Normal Version (Gemini Flash 1.5)", "Pro Version (Gemini Pro 1.5)")), y = Q_exp_to_pi_nash_ratio)) +
  geom_boxplot(fill = c("#66c2a5", "#fc8d62")) +
  labs(
    x = "Group",
    y = "Average Quantity per Round in Experiment to Nash Profit Ratio ",
    title = "Ratio of Average Quantity in the Experiments in Comparison to Nash Profits by Version"
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0.6, 1.3), labels = scales::percent)




#Quantity mit number of Firms Balkendiagramm

# Erstellen des Balkendiagramms
ggplot(df_oligopol_same_setting, aes(x = factor(n), y = r, fill = factor(is_pro, labels = c("Normal Version (Gemini Flash 1.5)", "Pro Version (Gemini Pro 1.5)")))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  labs(
    x = "Number of Firms",  # Beschriftung der X-Achse
    y = "Ø-Experimental Quantity to Nash-Quantity Ratio",  # Beschriftung der Y-Achse
    title = "Ratio of Experimental Quantity to Nash Quantity with increasing number of Firms",  # Titel des Diagramms
    fill = "Group"  # Legende für die Balkengruppen
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 1.2), breaks = c(0.3,0.6,0.9,1.2)) +
  
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Stil des Titels
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  )
