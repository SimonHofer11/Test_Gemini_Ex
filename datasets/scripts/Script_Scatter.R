library(readr)
library(dplyr)
library(tidyr)

setwd("C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/datasets/variants_Q_per_round")

df_final_all = readRDS("./ALL.Rds")

df_final_all$type <- ifelse(df_final_all$type == "NORMAL", "FLASH", df_final_all$type)


# Laden der notwendigen Bibliotheken
library(ggplot2)
library(dplyr)
df_final_all = df_final_all %>% 
  mutate(p = 100-Q) 
df_final_all
df_final_all$p[df_final_all$p < 0] <- 0

df_final_all = df_final_all %>% 
  mutate (pi = round((p*Q/n)-Q/n,2))







df_final_all$nash_Q <- ifelse(df_final_all$n == 2, 66, 
                            ifelse(df_final_all$n == 3, 74.25, 
                                   ifelse(df_final_all$n == 5, 82.5, NA)))

df_final_all$nash_pi <- ifelse(df_final_all$n == 2, 1089, 
                            ifelse(df_final_all$n == 3, 612.56, 
                                   ifelse(df_final_all$n == 5, 272.25, NA)))



df_final_all_summarized = df_final_all %>% 
  group_by(n, type) %>% 
  mutate(Q_ratio_to_nash = mean(Q)/nash_Q,
         pi_ratio_to_nash = mean(pi)/nash_pi)
  

###########2Firms#############
#
# Filtern der Daten für n == 2
df_2 <- df_final_all %>%
  filter(n == 2)


# Erstellen des Scatterplots mit Linien und horizontalen Linien
plot_2_q =  ggplot(data = df_2, aes(x = t, y = Q, color = type, group = type)) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_line() +
  geom_hline(yintercept = 49.5, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 66, linetype = "dashed", color = "black") +
  annotate("text", x = max(df_2$t) - 2, y = 49.5, label = "Collusion", vjust = 1.2, hjust = 1.8, size = 4) +
  annotate("text", x = max(df_2$t) - 2, y = 66, label = "Nash", vjust = 1.2, hjust = 3, size = 4) +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Average Quantity Set by Round",
    subtitle = "Oligopoly Experiment with 2 Firms" ,
    x = "Round",
    y = "Average Quantity Set",
    color = "Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    
    legend.position = "right"
  )


  
  #PI
  
  
  # Erstellen des Scatterplots mit Linien und horizontalen Linien
  plot_2_pi = ggplot(data = df_2, aes(x = t, y = pi, color = type, group = type)) +
    geom_point(size = 2.5, alpha = 0.8) +
    geom_line() +
    geom_hline(yintercept = 1225, linetype = "dashed", color = "black") +
    geom_hline(yintercept = 1089, linetype = "dashed", color = "black") +
    annotate("text", x = max(df_2$t) - 2, y = 1225, label = "Collusion", vjust = 1.2, hjust = 1.8, size = 4) +
    annotate("text", x = max(df_2$t) - 2, y = 1089, label = "Nash", vjust = 1.2, hjust = 3, size = 4) +
    scale_color_brewer(palette = "Set2") +
    labs(
      title = "Average Profit by Round",
      subtitle = "Oligopoly Experiment with 2 Firms" ,
      x = "Round",
      y = "Average Profit",
      color = "Type"
    ) +
    coord_cartesian(ylim = c(800, 1250)) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      
      legend.position = "right"
    )
  
  
  
  
  
  
  
  
  
  
  
  ###############3Firms############
df_3 <- df_final_all %>%
  filter(n == 3)


# Erstellen des Scatterplots mit Linien und horizontalen Linien
  plot_3_q = ggplot(data = df_3, aes(x = t, y = Q, color = type, group = type)) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_line() +
  geom_hline(yintercept = 49.5, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 74.25, linetype = "dashed", color = "black") +
  annotate("text", x = max(df_2$t) - 2, y = 49.5, label = "Collusion", vjust = 1.2, hjust = 0.8, size = 4) +
  annotate("text", x = max(df_2$t) - 2, y = 74.25, label = "Nash", vjust = 1.2, hjust = 1, size = 4) +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Average Quantity Set by Round",
    subtitle = "Oligopoly Experiment with 3 Firms" ,
    x = "Round",
    y = "Average Quantity Set",
    color = "Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "right"
  )


#PI


# Erstellen des Scatterplots mit Linien und horizontalen Linien
plot_3_pi = ggplot(data = df_3, aes(x = t, y = pi, color = type, group = type)) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_line() +
  geom_hline(yintercept = 816, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 612, linetype = "dashed", color = "black") +
  annotate("text", x = max(df_2$t) - 2, y = 816, label = "Collusion", vjust = 1.2, hjust = 1.8, size = 4) +
  annotate("text", x = max(df_2$t) - 2, y = 612, label = "Nash", vjust = 1.2, hjust = 3, size = 4) +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Average Profit by Round",
    subtitle = "Oligopoly Experiment with 3 Firms" ,
    x = "Round",
    y = "Average Profit",
    color = "Type"
  ) +
  coord_cartesian(ylim = c(0, 900)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    
    legend.position = "right"
  )




###############5Firms############



df_5 <- df_final_all %>%
  filter(n == 5)

plot_5_q = ggplot(data = df_5, aes(x = t, y = Q, color = type, group = type)) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_line() +
  geom_hline(yintercept = 49.5, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 74.25, linetype = "dashed", color = "black") +
  annotate("text", x = max(df_5$t) - 2, y = 49.5, label = "Collusion", vjust = 1.2, hjust = 0.6, size = 4) +
  annotate("text", x = max(df_5$t) - 2, y = 74.25, label = "Nash", vjust = 1.2, hjust = 0.6, size = 4) +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Average Quantity Set by Round",
    subtitle = "Oligopoly Experiment with 5 Firms",
    x = "Round",
    y = "Average Quantity Set",
    color = "Type"
  ) +
  coord_cartesian(ylim = c(0, 160)) + # Beschneidung der Ansicht, aber Beibehaltung aller Daten
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    
    legend.position = "right"
  )



# Erstellen des Scatterplots mit Linien und horizontalen Linien
plot_5_pi = ggplot(data = df_5, aes(x = t, y = pi, color = type, group = type)) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_line() +
  geom_hline(yintercept = 490, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 272, linetype = "dashed", color = "black") +
  annotate("text", x = max(df_2$t) - 2, y = 490, label = "Collusion", vjust = 1.2, hjust = 1.8, size = 4) +
  annotate("text", x = max(df_2$t) - 2, y = 272, label = "Nash", vjust = 1.2, hjust = 3, size = 4) +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Average Profit by Round",
    subtitle = "Oligopoly Experiment with 5 Firms" ,
    x = "Round",
    y = "Average Profit",
    color = "Type"
  ) +
  coord_cartesian(ylim = c(0, 900)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    
    legend.position = "right"
  )






library(patchwork)



join = plot_2_q / plot_2_pi
join











#########Bar Plot##############
library(ggplot2)

# Barplot erstellen
# Notwendige Bibliothek laden
library(ggplot2)

# Barplot erstellen
bar_plot_Q = ggplot(df_final_all_summarized, aes(x = factor(n), y = Q_ratio_to_nash, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") + # Bars nebeneinander
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") + 
  scale_fill_brewer(palette = "Set1") + # ggplot Farbpalette
  labs(
    title = expression("Comparison of " ~ bar(Q) ~ "to Nash Quantity"),
    subtitle = "Comparison by Number of Firms and Type of Agent",# Ansprechender Titel
    x = "Number of Firms", # X-Achsentitel
    y = expression("Ratio of " ~ bar(Q) ~ "to Nash Quantity"), # Y-Achsentitel mit Querstreifen auf Q
    fill = "Type"
  ) +
  scale_y_continuous(breaks = c(0,0.3,0.6,0.8,1,1.1), limits = c(0,1.15)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5), # Zentriert den Titel
    plot.subtitle  = element_text(hjust = 0.5) # Zentriert den Titel
    
  )



# Barplot erstellen
bar_plot_pi = ggplot(df_final_all_summarized, aes(x = factor(n), y = pi_ratio_to_nash, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") + # Bars nebeneinander
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") + 
  scale_fill_brewer(palette = "Set1") + # ggplot Farbpalette
  labs(
    title = expression("Comparison of " ~ bar(pi) ~ "to Nash Profits"),
    subtitle = "Comparison by Number of Firms and Type of Agent",# Ansprechender Titel
    x = "Number of Firms", # X-Achsentitel
    y = expression("Ratio of " ~ bar(pi) ~ "to Nash Profits"), # Y-Achsentitel mit Querstreifen auf Q
    fill = "Type"
  ) +
  scale_y_continuous(breaks = c(0,0.3,0.6,0.8,1,1.1,1.3,1.5), limits = c(0,1.5)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5), # Zentriert den Titel
    plot.subtitle  = element_text(hjust = 0.5) # Zentriert den Titel
    
  )



join = bar_plot_Q / bar_plot_pi
join




##########ALT Data Frame generierung######

# verzeichnis = "C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/datasets/variants_Q_per_round"
# 
# 
# # Listen Sie alle .Rds-Dateien im Verzeichnis auf
# datei_liste <- list.files(path = verzeichnis, pattern = "\\.Rds$", full.names = TRUE)
# 
# # Schleife über jede Datei
# for (datei in datei_liste) {
#   # Extrahieren Sie den Basisnamen ohne Erweiterung
#   basis_name <- tools::file_path_sans_ext(basename(datei))
#   
#   # Konvertieren Sie den Basisnamen in einen gültigen R-Variablennamen
#   basis_name <- make.names(basis_name)
#   
#   # Lesen Sie die Rds-Datei ein
#   daten <- readRDS(datei)
#   
#   # Erstellen Sie den Variablennamen df_final_X
#   variablen_name <- paste0("df_final_", basis_name)
#   
#   # Weisen Sie die Daten der Variablen zu
#   assign(variablen_name, daten)
# }
# 
# 
# run <- rep(c(1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 6), each = 25)
# 
# df_final_df_2_Firms_ALL_Observations_NORMAL$run = run
# df_final_df_2_Firms_ALL_Observations_PRO$run = run
# 
# 
# 
# run <- rep(c(1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 6,1,2,3,4,5,6), each = 25)
# 
# df_final_df_3_Firms_ALL_Observations_NORMAL$run = run
# df_final_df_3_Firms_ALL_Observations_PRO$run = run 
# 
# run <- rep(c(1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6), each = 25)
# 
# 
# df_final_df_5_Firms_ALL_Observations_NORMAL$run = run
# df_final_df_5_Firms_ALL_Observations_PRO$run = run
# 
# 
# 
# df_final_2_firms_ALL_Observations_NORMAL_calc = df_final_df_2_Firms_ALL_Observations_NORMAL %>% 
#   group_by(t) %>% 
#   summarize(Q = mean(Q),
#             pi = mean(pi),
#             p = mean(p))
# 
# df_final_2_firms_ALL_Observations_PRO_calc = df_final_df_2_Firms_ALL_Observations_PRO %>% 
#   group_by(t) %>% 
#   summarize(Q = mean(Q),
#             pi = mean(pi),
#             p = mean(p))
# 
# df_final_3_firms_ALL_Observations_NORMAL_calc = df_final_df_3_Firms_ALL_Observations_NORMAL %>% 
#   group_by(t) %>% 
#   summarize(Q = mean(Q),
#             pi = mean(pi),
#             p = mean(p))
# 
# df_final_3_firms_ALL_Observations_PRO_calc = df_final_df_3_Firms_ALL_Observations_PRO %>% 
#   group_by(t) %>% 
#   summarize(Q = mean(Q),
#             pi = mean(pi),
#             p = mean(p))
# 
# df_final_5_firms_ALL_Observations_NORMAL_calc = df_final_df_5_Firms_ALL_Observations_NORMAL %>% 
#   group_by(t) %>% 
#   summarize(Q = mean(Q),
#             pi = mean(pi),
#             p = mean(p))
# 
# df_final_5_firms_ALL_Observations_PRO_calc = df_final_df_5_Firms_ALL_Observations_PRO %>% 
#   group_by(t) %>% 
#   summarize(Q = mean(Q),
#             pi = mean(pi),
#             p = mean(p))
# 
# 
# df_final_2_firms_ALL_Observations_NORMAL_calc$type = "NORMAL"
# df_final_2_firms_ALL_Observations_PRO_calc$type = "PRO"
# df_final_3_firms_ALL_Observations_NORMAL_calc$type = "NORMAL"
# df_final_3_firms_ALL_Observations_PRO_calc$type = "PRO"
# df_final_5_firms_ALL_Observations_NORMAL_calc$type = "NORMAL"
# df_final_5_firms_ALL_Observations_PRO_calc$type = "PRO"
# 
# 
# df_final_human_data$Q = df_final_human_data$totalquantity
# df_final_human_data$t = df_final_human_data$PERIOD
# 
# df_final_human_data_calc = df_final_human_data %>% 
#   group_by(TREATM,t) %>% 
#   summarize(Q = mean(Q)) %>% 
#   select(t,Q) %>% 
#   mutate(type = "HUMAN") %>% 
#   ungroup()
# 
# 
# df_final_human_2_data_calc= df_final_human_data_calc %>% 
#   filter(TREATM==2)
# 
# df_final_human_3_data_calc= df_final_human_data_calc %>% 
#   filter(TREATM==3)
# 
# df_final_human_5_data_calc= df_final_human_data_calc %>% 
#   filter(TREATM==5)
# 
# 
# 
# ###Nur die Q extrahieren
# df_final_2_ALL = rbind(select(df_final_2_firms_ALL_Observations_NORMAL_calc,t,Q,type),select(df_final_2_firms_ALL_Observations_PRO_calc,t,Q,type),select(df_final_human_2_data_calc,t,Q,type))
# df_final_2_ALL$n = 2
# 
# df_final_3_ALL = rbind(select(df_final_3_firms_ALL_Observations_NORMAL_calc,t,Q,type),select(df_final_3_firms_ALL_Observations_PRO_calc,t,Q,type),select(df_final_human_3_data_calc,t,Q,type))
# df_final_3_ALL$n = 3
# 
# 
# df_final_5_ALL = rbind(select(df_final_5_firms_ALL_Observations_NORMAL_calc,t,Q,type),select(df_final_5_firms_ALL_Observations_PRO_calc,t,Q,type),select(df_final_human_5_data_calc,t,Q,type))
# 
# df_final_5_ALL$n = 5
# 
# 
# df_final_all = rbind(df_final_2_ALL,df_final_3_ALL,df_final_5_ALL)
# 
# 
# 
# saveRDS(df_final_all,"C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/datasets/variants_Q_per_round/ALL.Rds")











# 
# rds_files <- list.files(path = paste0(x),pattern = "\\.Rds$", full.names = TRUE)
# 
# ####################### ALT Datensatz herstellen#########################
# 
# 
# # Eine leere Liste für die Ergebnisse erstellen
# experiment_results <- list()
# 
# # Alle .RDS Dateien einlesen und in die Liste speichern mit benutzerdefinierten Namen
# experiment_results <- lapply(seq_along(rds_files), function(i) {
#   readRDS(rds_files[i])
# })
# 
# 
# 
# 
# # Leeren Dataframe erstellen, der die extrahierten Daten aufnehmen wird
# final_df_player_1 <- data.frame()
# final_df_player_2 <- data.frame()
# 
# # Schleife über alle "game_2_player_run" in experiment_results[[1]]
# for (run_index in 1:6) {
#   # Zugriff auf die Unterliste "game_2_player_run_i"
#   current_run <- experiment_results[[1]][[paste0("game_2_player_run_", run_index)]]
#   # Extrahieren der Spalten i, p, q, pi, Q aus der Liste "player_dfs"
#   extracted_data_player_1 <- data.frame(
#     i = current_run$player_dfs[[1]]$i,
#     t = current_run$player_dfs[[1]]$t,
#     p = current_run$player_dfs[[1]]$p,
#     q = current_run$player_dfs[[1]]$q,
#     pi = current_run$player_dfs[[1]]$pi,
#     Q = current_run$player_dfs[[1]]$Q
#   )
#   
#   extracted_data_player_2 <- data.frame(
#     i = current_run$player_dfs[[2]]$i,
#     t = current_run$player_dfs[[2]]$t,
#     p = current_run$player_dfs[[2]]$p,
#     q = current_run$player_dfs[[2]]$q,
#     pi = current_run$player_dfs[[2]]$pi,
#     Q = current_run$player_dfs[[2]]$Q
#   )
#   
#   # Zusammenfügen der extrahierten Daten zum finalen Dataframe
#   final_df_player_1 <- rbind(final_df_player_1, extracted_data_player_1)
#   final_df_player_2 <- rbind(final_df_player_2, extracted_data_player_2)
# }
# 
# final_df = bind_rows(final_df_player_1,final_df_player_2)
# 
# #saveRDS(object = final_df, file = "C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/gemini_results/Final_df/df_2_Firms_ALL_Observations_PRO.Rds")
# # Der finale Dataframe "fin
# 
# 

final_df = readRDS("C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/datasets/variants_Q_per_round/df_2_Firms_ALL_Observations_NORMAL.Rds")

final_df_plot = final_df %>% 
  group_by(i,t) %>% 
  summarize(q_mean = mean(q),
            p_mean = mean(p),
            pi_mean = mean(pi)) 

final_df_plot_Q = final_df %>% 
  group_by(t) %>% 
  summarize(Q = mean(Q))


############QUANTITY PRO#####



library(ggplot2)

# Erstellung des Punktediagramms
 plot_pro = ggplot(final_df_plot_Q, aes(x = t, y = Q)) +
  geom_point(size = 3) +  # Punkte plotten
  geom_line(size = 1) +  # Linie zwischen den Punkten erstellen
  geom_hline(yintercept = 66, linetype = "dashed", color = "blue", size = 1) +  # Linie bei q_mean = 33
  geom_text(aes(x = max(t) * 0.9, y = 66, label = "Nash Quantity"), color = "blue", vjust = -1) +
  geom_hline(yintercept = 49.5, linetype = "dashed", color = "red", size = 1) +  # Linie bei q_mean = 24.75
  geom_text(aes(x = max(t) * 0.9, y = 49.5, label = "Collusive Quantity"), color = "red", vjust = -1) +
  labs(
    x = "Round",  # Beschriftung der X-Achse
    y = "Average Quantity set",  # Beschriftung der Y-Achse
    color = "Group",  # Legende für die Gruppen
    title = "Average Quantity Set by Round",
    subtitle = "Oligopoly Experiment with 2 Firms and Gemini Version Pro 1.5"# Titel des Plots
    # Titel des Plots
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Stil des Titels
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5)  # Stil des Untertitels und zentriert
    
  )  
plot_pro


final_df_plot_Q_pro = final_df_plot_Q


###PROFIT PRO

final_df_plot_pi = final_df %>% 
  group_by(t) %>% 
  summarize(pi = mean(pi))

# Erstellung des Punktediagramms
plot_pi_pro = ggplot(final_df_plot_pi, aes(x = t, y = pi)) +
  geom_point(size = 3) +  # Punkte plotten
  geom_line(size = 1) +  # Linie zwischen den Punkten erstellen
  geom_hline(yintercept = 1089, linetype = "dashed", color = "blue", size = 1) +  # Linie bei q_mean = 33
  geom_text(aes(x = max(t) * 0.9, y = 1089, label = "Nash Quantity"), color = "blue", vjust = -1) +
  geom_hline(yintercept = 1225.13, linetype = "dashed", color = "red", size = 1) +  # Linie bei q_mean = 24.75
  geom_text(aes(x = max(t) * 0.9, y = 1225.13, label = "Collusive Quantity"), color = "red", vjust = -1) +
  labs(
    x = "Round",  # Beschriftung der X-Achse
    y = "Average Quantity set",  # Beschriftung der Y-Achse
    color = "Group",  # Legende für die Gruppen
    title = "Average Quantity Set by Round",
    subtitle = "Oligopoly Experiment with 2 Firms and Gemini Version Pro 1.5"# Titel des Plots
    # Titel des Plots
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Stil des Titels
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5)  # Stil des Untertitels und zentriert
    
  )  
plot_pi_pro



final_df_plot_pi_PRO = final_df_plot_pi













x = "C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/gemini_results/Oligopoly/2_Firms_25_rounds/2_Firms_25_Rounds_w_prev_strat"



rds_files <- list.files(path = paste0(x),pattern = "\\.Rds$", full.names = TRUE)



# Eine leere Liste für die Ergebnisse erstellen
experiment_results <- list()

# Alle .RDS Dateien einlesen und in die Liste speichern mit benutzerdefinierten Namen
experiment_results <- lapply(seq_along(rds_files), function(i) {
  readRDS(rds_files[i])
})



# Leeren Dataframe erstellen, der die extrahierten Daten aufnehmen wird
final_df_player_1 <- data.frame()
final_df_player_2 <- data.frame()

# Schleife über alle "game_2_player_run" in experiment_results[[1]]
for (run_index in 1:6) {
  # Zugriff auf die Unterliste "game_2_player_run_i"
  current_run <- experiment_results[[1]][[1]][[paste0("game_2_player_run_", run_index)]]
  # Extrahieren der Spalten i, p, q, pi, Q aus der Liste "player_dfs"
  extracted_data_player_1 <- data.frame(
    i = current_run$player_dfs[[1]]$i,
    t = current_run$player_dfs[[1]]$t,
    p = current_run$player_dfs[[1]]$p,
    q = current_run$player_dfs[[1]]$q,
    pi = current_run$player_dfs[[1]]$pi,
    Q = current_run$player_dfs[[1]]$Q
  )
  
  extracted_data_player_2 <- data.frame(
    i = current_run$player_dfs[[2]]$i,
    t = current_run$player_dfs[[2]]$t,
    p = current_run$player_dfs[[2]]$p,
    q = current_run$player_dfs[[2]]$q,
    pi = current_run$player_dfs[[2]]$pi,
    Q = current_run$player_dfs[[2]]$Q
  )
  
  # Zusammenfügen der extrahierten Daten zum finalen Dataframe
  final_df_player_1 <- rbind(final_df_player_1, extracted_data_player_1)
  final_df_player_2 <- rbind(final_df_player_2, extracted_data_player_2)
}

final_df = bind_rows(final_df_player_1,final_df_player_2)

saveRDS(object = final_df, file = "C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/gemini_results/Final_df/df_2_Firms_ALL_Observations_NORMAL.Rds")
# Der finale Dataframe "fin

final_df_plot = final_df %>% 
  group_by(i,t) %>% 
  summarize(q_mean = mean(q),
            p_mean = mean(p),
            pi_mean = mean(pi)) 

final_df_plot_Q = final_df %>% 
  group_by(t) %>% 
  summarize(Q = mean(Q))

final_df_plot_Q$Q_Pro = final_df_plot_Q_pro$Q

final_df_plot_pi_normal <- final_df_plot

library(ggplot2)



plot_normal = ggplot(final_df_plot_Q, aes(x = t)) +
  geom_point(aes(y = Q, color = "Normal Version"), size = 3) +  # Punkte plotten für Q
  geom_line(aes(y = Q, color = "Normal Version"), size = 1) +  # Linie zwischen den Punkten für Q
  geom_hline(yintercept = 66, linetype = "dashed", color = "blue", size = 1) +  # Linie bei Q = 66
  geom_text(aes(x = max(t) * 0.9, y = 66, label = "Nash Quantity"), color = "blue", vjust = -1) +
  geom_hline(yintercept = 49.5, linetype = "dashed", color = "red", size = 1) +  # Linie bei Q = 49.5
  geom_text(aes(x = max(t) * 0.9, y = 49.5, label = "Collusive Quantity"), color = "red", vjust = -1) +
  geom_point(aes(y = Q_Pro, color = "Pro Version"), size = 3, shape = 17) +  # Zusätzlicher Scatterplot für Q_Pro
  geom_line(aes(y = Q_Pro, color = "Pro Version"), size = 1) +  # Linie für Q_Pro
  labs(
    x = "Round",  # Beschriftung der X-Achse
    y = "Average Quantity set",  # Beschriftung der Y-Achse
    color = "Version",  # Legende für die Gruppen
    title = "Average Quantity Set by Round",
    subtitle = "Oligopoly Experiment with 2 Firms and Different Gemini Versions"  # Titel des Plots
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 100), breaks = c(0,20,40,60,80,100)) +  
  scale_x_continuous(breaks = c(1,5,10,15,20,25)) +  
  
  scale_color_manual(values = c("Normal Version" = "black", "Pro Version" = "green")) + 
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
    plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5),  # Stil des Untertitels und zentriert
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  )

# Plot anzeigen
print(plot_normal)



final_df_plot = final_df %>% 
  group_by(i,t) %>% 
  summarize(q_mean = mean(q),
            p_mean = mean(p),
            pi_mean = mean(pi)) 


final_df_plot_pi = final_df %>% 
  group_by(t) %>% 
  summarize(pi = mean(pi))


final_df_plot_pi$pi_PRO = final_df_plot_pi_PRO$pi








plot_pi_joined = ggplot(final_df_plot_pi, aes(x = t)) +
  geom_point(aes(y = pi, color = "Normal Version"), size = 3) +  # Punkte plotten für pi
  geom_line(aes(y = pi, color = "Normal Version"), size = 1) +  # Linie für pi
  geom_hline(yintercept = 1089, linetype = "dashed", color = "blue", size = 1) +  # Linie bei pi = 1089
  geom_text(aes(x = max(t) * 0.9, y = 1089, label = "Nash Profits"), color = "blue", vjust = -1) +
  geom_hline(yintercept = 1225.13, linetype = "dashed", color = "red", size = 1) +  # Linie bei pi = 1225.13
  geom_text(aes(x = max(t) * 0.9, y = 1225.13, label = "Collusive Profits"), color = "red", vjust = -1) +
  geom_point(aes(y = pi_PRO, color = "Pro Version"), size = 3, shape = 17) +  # Punkte plotten für pi_PRO
  geom_line(aes(y = pi_PRO, color = "Pro Version"), size = 1) +  # Linie für pi_PRO
  labs(
    x = "Round",  # Beschriftung der X-Achse
    y = "Average Profits",  # Beschriftung der Y-Achse
    color = "Version",  # Legende für die Gruppen
    title = "Average Profits by Round",
    subtitle = "Oligopoly Experiment with 2 Firms and Different Gemini Versions"  # Untertitel des Plots
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 1400), breaks = c(0,500,800,1000,1200,1400)) +  
  scale_x_continuous(breaks = c(1,5,10,15,20,25)) +  
  
  scale_color_manual(values = c("Normal Version" = "black", "Pro Version" = "green")) +  # Farben für die Versionen definieren
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Stil des Titels
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5)  # Stil des Untertitels und zentriert
  )

# Plot anzeigen
print(plot_pi_joined)























#######################3FIRMEN#########################



#Oligopol 3 Firmen 
x = "C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/gemini_results/Oligopoly/3_Firms_25_rounds"


#Oligopol 3 Firmen PRO Version
#x = "C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/gemini_results/Oligopoly/3_Firms_25_rounds_PRO"









rds_files <- list.files(path = paste0(x),pattern = "\\.Rds$", full.names = TRUE)



# Eine leere Liste für die Ergebnisse erstellen
experiment_results <- list()

# Alle .RDS Dateien einlesen und in die Liste speichern mit benutzerdefinierten Namen
experiment_results <- lapply(seq_along(rds_files), function(i) {
  readRDS(rds_files[i])
})



# Leeren Dataframe erstellen, der die extrahierten Daten aufnehmen wird
final_df_player_1 <- data.frame()
final_df_player_2 <- data.frame()
final_df_player_3 <- data.frame()




# Schleife über alle "game_2_player_run" in experiment_results[[1]]
for (run_index in 1:6) {
  # Zugriff auf die Unterliste "game_2_player_run_i"
  current_run <- experiment_results[[1]][[paste0("game_3_player_run_", run_index)]]
  # Extrahieren der Spalten i, p, q, pi, Q aus der Liste "player_dfs"
  extracted_data_player_1 <- data.frame(
    i = current_run$player_dfs[[1]]$i,
    t = current_run$player_dfs[[1]]$t,
    p = current_run$player_dfs[[1]]$p,
    q = current_run$player_dfs[[1]]$q,
    pi = current_run$player_dfs[[1]]$pi,
    Q = current_run$player_dfs[[1]]$Q
  )
  
  extracted_data_player_2 <- data.frame(
    i = current_run$player_dfs[[2]]$i,
    t = current_run$player_dfs[[2]]$t,
    p = current_run$player_dfs[[2]]$p,
    q = current_run$player_dfs[[2]]$q,
    pi = current_run$player_dfs[[2]]$pi,
    Q = current_run$player_dfs[[2]]$Q
  )
  
  
  extracted_data_player_3 <- data.frame(
    i = current_run$player_dfs[[3]]$i,
    t = current_run$player_dfs[[3]]$t,
    p = current_run$player_dfs[[3]]$p,
    q = current_run$player_dfs[[3]]$q,
    pi = current_run$player_dfs[[3]]$pi,
    Q = current_run$player_dfs[[3]]$Q
  )
  # Zusammenfügen der extrahierten Daten zum finalen Dataframe
  final_df_player_1 <- rbind(final_df_player_1, extracted_data_player_1)
  final_df_player_2 <- rbind(final_df_player_2, extracted_data_player_2)
  final_df_player_3 <- rbind(final_df_player_3, extracted_data_player_3)
  
}

final_df = bind_rows(final_df_player_1,final_df_player_2, final_df_player_3)

saveRDS(object = final_df, file = "C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/gemini_results/Final_df/df_3_Firms_ALL_Observations_NORMAL.Rds")
# Der finale Dataframe "fin

final_df_plot = final_df %>% 
  group_by(i,t) %>% 
  summarize(q_mean = mean(q),
            p_mean = mean(p),
            pi_mean = mean(pi)) 

final_df_plot_Q = final_df %>% 
  group_by(t) %>% 
  summarize(Q = mean(Q))


final_df_plot_pi = final_df %>% 
  group_by(t) %>% 
  summarize(pi = mean(pi))


###Scatter Plot: Alle 3 Firmen als Streudiagramm
#Noch nicht vereint auf Alle vs Pro


library(ggplot2)

# Erstellung des Punktediagramms
plot_pro = ggplot(final_df_plot, aes(x = t, y = q_mean, color = as.factor(i))) +
  geom_point(size = 3) +  # Punkte plotten
  geom_line(size = 1) +  # Linie zwischen den Punkten erstellen
  geom_hline(yintercept = 24.75, linetype = "dashed", color = "blue", size = 1) +  # Linie bei q_mean = 33
  geom_text(aes(x = max(t) * 0.9, y = 24.75, label = "Nash Quantity"), color = "blue", vjust = -1) +
  geom_hline(yintercept = 49.5/3, linetype = "dashed", color = "black", size = 1) +  # Linie bei q_mean = 24.75
  geom_text(aes(x = max(t) * 0.9, y = 49.5/3, label = "Collusive Quantity"), color = "black", vjust = -1) +
  labs(
    x = "Round",  # Beschriftung der X-Achse
    y = "Average Quantity set",  # Beschriftung der Y-Achse
    color = "Group",  # Legende für die Gruppen
    title = "Average Quantity per Firm in each Round",
    subtitle = "Oligopoly Experiment with 3 Firms and Gemini Version Flash 1.5"# Titel des Plots
    # Titel des Plots
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Stil des Titels
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5)  # Stil des Untertitels und zentriert
    
  )  
plot_pro




#########Jetzt PRO hinzufügen.





#Oligopol 3 Firmen PRO Version
x = "C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/gemini_results/Oligopoly/3_Firms_25_rounds_PRO"









rds_files <- list.files(path = paste0(x),pattern = "\\.Rds$", full.names = TRUE)



# Eine leere Liste für die Ergebnisse erstellen
experiment_results <- list()

# Alle .RDS Dateien einlesen und in die Liste speichern mit benutzerdefinierten Namen
experiment_results <- lapply(seq_along(rds_files), function(i) {
  readRDS(rds_files[i])
})



# Leeren Dataframe erstellen, der die extrahierten Daten aufnehmen wird
final_df_player_1 <- data.frame()
final_df_player_2 <- data.frame()
final_df_player_3 <- data.frame()




# Schleife über alle "game_2_player_run" in experiment_results[[1]]
for (run_index in 1:6) {
  # Zugriff auf die Unterliste "game_2_player_run_i"
  current_run <- experiment_results[[1]][[paste0("game_3_player_run_", run_index)]]
  # Extrahieren der Spalten i, p, q, pi, Q aus der Liste "player_dfs"
  extracted_data_player_1 <- data.frame(
    i = current_run$player_dfs[[1]]$i,
    t = current_run$player_dfs[[1]]$t,
    p = current_run$player_dfs[[1]]$p,
    q = current_run$player_dfs[[1]]$q,
    pi = current_run$player_dfs[[1]]$pi,
    Q = current_run$player_dfs[[1]]$Q
  )
  
  extracted_data_player_2 <- data.frame(
    i = current_run$player_dfs[[2]]$i,
    t = current_run$player_dfs[[2]]$t,
    p = current_run$player_dfs[[2]]$p,
    q = current_run$player_dfs[[2]]$q,
    pi = current_run$player_dfs[[2]]$pi,
    Q = current_run$player_dfs[[2]]$Q
  )
  
  
  extracted_data_player_3 <- data.frame(
    i = current_run$player_dfs[[3]]$i,
    t = current_run$player_dfs[[3]]$t,
    p = current_run$player_dfs[[3]]$p,
    q = current_run$player_dfs[[3]]$q,
    pi = current_run$player_dfs[[3]]$pi,
    Q = current_run$player_dfs[[3]]$Q
  )
  # Zusammenfügen der extrahierten Daten zum finalen Dataframe
  final_df_player_1 <- rbind(final_df_player_1, extracted_data_player_1)
  final_df_player_2 <- rbind(final_df_player_2, extracted_data_player_2)
  final_df_player_3 <- rbind(final_df_player_3, extracted_data_player_3)
  
}

final_df = bind_rows(final_df_player_1,final_df_player_2, final_df_player_3)

saveRDS(object = final_df, file = "C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/gemini_results/Final_df/df_3_Firms_ALL_Observations_PRO.Rds")
# Der finale Dataframe "fin

final_df_plot_PRO = final_df %>% 
  group_by(i,t) %>% 
  summarize(q_mean = mean(q),
            p_mean = mean(p),
            pi_mean = mean(pi)) 

final_df_plot_Q_PRO = final_df %>% 
  group_by(t) %>% 
  summarize(Q = mean(Q))


final_df_plot_pi_PRO = final_df %>% 
  group_by(t) %>% 
  summarize(pi = mean(pi))


final_df_plot_Q$Q_PRO = final_df_plot_Q_PRO$Q
final_df_plot_pi$pi_PRO = final_df_plot_pi_PRO$pi











###Quantity





plot_q_3_Firms = ggplot(final_df_plot_Q, aes(x = t)) +
  geom_point(aes(y = Q, color = "Normal Version"), size = 3) +  # Punkte plotten für Q
  geom_line(aes(y = Q, color = "Normal Version"), size = 1) +  # Linie zwischen den Punkten für Q
  geom_hline(yintercept = 74.25, linetype = "dashed", color = "blue", size = 1) +  # Linie bei Q = 66
  geom_text(aes(x = max(t) * 0.9, y = 74.25, label = "Nash Quantity"), color = "blue", vjust = -1) +
  geom_hline(yintercept = 49.5, linetype = "dashed", color = "red", size = 1) +  # Linie bei Q = 49.5
  geom_text(aes(x = max(t) * 0.9, y = 49.5, label = "Collusive Quantity"), color = "red", vjust = -1) +
  geom_point(aes(y = Q_PRO, color = "Pro Version"), size = 3, shape = 17) +  # Zusätzlicher Scatterplot für Q_Pro
  geom_line(aes(y = Q_PRO, color = "Pro Version"), size = 1) +  # Linie für Q_Pro
  labs(
    x = "Round",  # Beschriftung der X-Achse
    y = "Average Quantity set",  # Beschriftung der Y-Achse
    color = "Version",  # Legende für die Gruppen
    title = "Average Quantity Set by Round",
    subtitle = "Oligopoly Experiment with 3 Firms and Different Gemini Versions"  # Titel des Plots
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(30, 125), breaks = c(30,40,60,80,100,125)) +  
  scale_x_continuous(breaks = c(1,5,10,15,20,25)) +  
  
  scale_color_manual(values = c("Normal Version" = "black", "Pro Version" = "green")) + 
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
    plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5),  # Stil des Untertitels und zentriert
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  )

# Plot anzeigen
print(plot_q_3_Firms)





###PI


plot_pi_3_firms = ggplot(final_df_plot_pi, aes(x = t)) +
  geom_point(aes(y = pi, color = "Normal Version"), size = 3) +  # Punkte plotten für pi
  geom_line(aes(y = pi, color = "Normal Version"), size = 1) +  # Linie für pi
  geom_hline(yintercept = 612.56, linetype = "dashed", color = "blue", size = 1) +  # Linie bei pi = 1089
  geom_text(aes(x = max(t) * 0.9, y = 612.56, label = "Nash Profits"), color = "blue", vjust = -1) +
  geom_hline(yintercept = 816.75, linetype = "dashed", color = "red", size = 1) +  # Linie bei pi = 1225.13
  geom_text(aes(x = max(t) * 0.9, y = 816.75, label = "Collusive Profits"), color = "red", vjust = -1) +
  geom_point(aes(y = pi_PRO, color = "Pro Version"), size = 3, shape = 17) +  # Punkte plotten für pi_PRO
  geom_line(aes(y = pi_PRO, color = "Pro Version"), size = 1) +  # Linie für pi_PRO
  labs(
    x = "Round",  # Beschriftung der X-Achse
    y = "Average Profits",  # Beschriftung der Y-Achse
    color = "Version",  # Legende für die Gruppen
    title = "Average Profits by Round",
    subtitle = "Oligopoly Experiment with 3 Firms and Different Gemini Versions"  # Untertitel des Plots
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(-100, 900), breaks = c(-100,0,500,800,900)) +  
  scale_x_continuous(breaks = c(1,5,10,15,20,25)) +  
  
  scale_color_manual(values = c("Normal Version" = "black", "Pro Version" = "green")) +  # Farben für die Versionen definieren
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Stil des Titels
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5)  # Stil des Untertitels und zentriert
  )

# Plot anzeigen
print(plot_pi_3_firms)
































#######################5FIRMEN#########################



#Oligopoly 5 firms 25 rounds with prev strategy
x = "C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/gemini_results/Oligopoly/5_Firms_25_rounds"

#Oligopoly 5 firms 25 rounds PRO
#x = "C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/gemini_results/Oligopoly/5_Firms_25_rounds_PRO"







rds_files <- list.files(path = paste0(x),pattern = "\\.Rds$", full.names = TRUE)



# Eine leere Liste für die Ergebnisse erstellen
experiment_results <- list()

# Alle .RDS Dateien einlesen und in die Liste speichern mit benutzerdefinierten Namen
experiment_results <- lapply(seq_along(rds_files), function(i) {
  readRDS(rds_files[i])
})



# Leeren Dataframe erstellen, der die extrahierten Daten aufnehmen wird
final_df_player_1 <- data.frame()
final_df_player_2 <- data.frame()
final_df_player_3 <- data.frame()
final_df_player_4 <- data.frame()
final_df_player_5 <- data.frame()



# Schleife über alle "game_2_player_run" in experiment_results[[1]]
for (run_index in 1:6) {
  # Zugriff auf die Unterliste "game_2_player_run_i"
  current_run <- experiment_results[[1]][[paste0("game_5_player_run_", run_index)]]
  # Extrahieren der Spalten i, p, q, pi, Q aus der Liste "player_dfs"
  extracted_data_player_1 <- data.frame(
    i = current_run$player_dfs[[1]]$i,
    t = current_run$player_dfs[[1]]$t,
    p = current_run$player_dfs[[1]]$p,
    q = current_run$player_dfs[[1]]$q,
    pi = current_run$player_dfs[[1]]$pi,
    Q = current_run$player_dfs[[1]]$Q
  )
  
  extracted_data_player_2 <- data.frame(
    i = current_run$player_dfs[[2]]$i,
    t = current_run$player_dfs[[2]]$t,
    p = current_run$player_dfs[[2]]$p,
    q = current_run$player_dfs[[2]]$q,
    pi = current_run$player_dfs[[2]]$pi,
    Q = current_run$player_dfs[[2]]$Q
  )
  
  
  extracted_data_player_3 <- data.frame(
    i = current_run$player_dfs[[3]]$i,
    t = current_run$player_dfs[[3]]$t,
    p = current_run$player_dfs[[3]]$p,
    q = current_run$player_dfs[[3]]$q,
    pi = current_run$player_dfs[[3]]$pi,
    Q = current_run$player_dfs[[3]]$Q
  )
  
  
  extracted_data_player_4 <- data.frame(
    i = current_run$player_dfs[[4]]$i,
    t = current_run$player_dfs[[4]]$t,
    p = current_run$player_dfs[[4]]$p,
    q = current_run$player_dfs[[4]]$q,
    pi = current_run$player_dfs[[4]]$pi,
    Q = current_run$player_dfs[[4]]$Q
  )
  
  
  extracted_data_player_5 <- data.frame(
    i = current_run$player_dfs[[5]]$i,
    t = current_run$player_dfs[[5]]$t,
    p = current_run$player_dfs[[5]]$p,
    q = current_run$player_dfs[[5]]$q,
    pi = current_run$player_dfs[[5]]$pi,
    Q = current_run$player_dfs[[5]]$Q
  )
  # Zusammenfügen der extrahierten Daten zum finalen Dataframe
  final_df_player_1 <- rbind(final_df_player_1, extracted_data_player_1)
  final_df_player_2 <- rbind(final_df_player_2, extracted_data_player_2)
  final_df_player_3 <- rbind(final_df_player_3, extracted_data_player_3)
  final_df_player_4 <- rbind(final_df_player_4, extracted_data_player_4)
  final_df_player_5 <- rbind(final_df_player_5, extracted_data_player_5)
}

final_df = bind_rows(final_df_player_1,final_df_player_2, final_df_player_3,final_df_player_4,final_df_player_5)

saveRDS(object = final_df, file = "C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/gemini_results/Final_df/df_5_Firms_ALL_Observations_NORMAL.Rds")
# Der finale Dataframe "fin

final_df_plot = final_df %>% 
  group_by(i,t) %>% 
  summarize(q_mean = mean(q),
            p_mean = mean(p),
            pi_mean = mean(pi)) 

final_df_plot_Q = final_df %>% 
  group_by(t) %>% 
  summarize(Q = mean(Q))


final_df_plot_pi = final_df %>% 
  group_by(t) %>% 
  summarize(pi = mean(pi))


###Scatter Plot: Alle 5 Firmen als Streudiagramm
#Noch nicht vereint auf Alle vs Pro


library(ggplot2)

# Erstellung des Punktediagramms
plot_pro = ggplot(final_df_plot, aes(x = t, y = q_mean, color = as.factor(i))) +
  geom_point(size = 3) +  # Punkte plotten
  geom_line(size = 1) +  # Linie zwischen den Punkten erstellen
  geom_hline(yintercept = 16.5, linetype = "dashed", color = "blue", size = 1) +  # Linie bei q_mean = 33
  geom_text(aes(x = max(t) * 0.9, y = 16.5, label = "Nash Quantity"), color = "blue", vjust = -1) +
  geom_hline(yintercept = 9.9, linetype = "dashed", color = "black", size = 1) +  # Linie bei q_mean = 24.75
  geom_text(aes(x = max(t) * 0.9, y = 9.9, label = "Collusive Quantity"), color = "black", vjust = -1) +
  labs(
    x = "Round",  # Beschriftung der X-Achse
    y = "Average Quantity set",  # Beschriftung der Y-Achse
    color = "Group",  # Legende für die Gruppen
    title = "Average Quantity per Firm in each Round",
    subtitle = "Oligopoly Experiment with 5 Firms and Gemini Version Flash 1.5"# Titel des Plots
    # Titel des Plots
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Stil des Titels
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5)  # Stil des Untertitels und zentriert
    
  )  
plot_pro




#########Jetzt PRO hinzufügen.



#Oligopoly 5 firms 25 rounds PRO
x = "C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/gemini_results/Oligopoly/5_Firms_25_rounds_PRO"







rds_files <- list.files(path = paste0(x),pattern = "\\.Rds$", full.names = TRUE)



# Eine leere Liste für die Ergebnisse erstellen
experiment_results <- list()

# Alle .RDS Dateien einlesen und in die Liste speichern mit benutzerdefinierten Namen
experiment_results <- lapply(seq_along(rds_files), function(i) {
  readRDS(rds_files[i])
})




# Leeren Dataframe erstellen, der die extrahierten Daten aufnehmen wird
final_df_player_1 <- data.frame()
final_df_player_2 <- data.frame()
final_df_player_3 <- data.frame()
final_df_player_4 <- data.frame()
final_df_player_5 <- data.frame()



# Schleife über alle "game_2_player_run" in experiment_results[[1]]
for (run_index in 1:6) {
  # Zugriff auf die Unterliste "game_2_player_run_i"
  current_run <- experiment_results[[1]][[paste0("game_5_player_run_", run_index)]]
  # Extrahieren der Spalten i, p, q, pi, Q aus der Liste "player_dfs"
  extracted_data_player_1 <- data.frame(
    i = current_run$player_dfs[[1]]$i,
    t = current_run$player_dfs[[1]]$t,
    p = current_run$player_dfs[[1]]$p,
    q = current_run$player_dfs[[1]]$q,
    pi = current_run$player_dfs[[1]]$pi,
    Q = current_run$player_dfs[[1]]$Q
  )
  
  extracted_data_player_2 <- data.frame(
    i = current_run$player_dfs[[2]]$i,
    t = current_run$player_dfs[[2]]$t,
    p = current_run$player_dfs[[2]]$p,
    q = current_run$player_dfs[[2]]$q,
    pi = current_run$player_dfs[[2]]$pi,
    Q = current_run$player_dfs[[2]]$Q
  )
  
  
  extracted_data_player_3 <- data.frame(
    i = current_run$player_dfs[[3]]$i,
    t = current_run$player_dfs[[3]]$t,
    p = current_run$player_dfs[[3]]$p,
    q = current_run$player_dfs[[3]]$q,
    pi = current_run$player_dfs[[3]]$pi,
    Q = current_run$player_dfs[[3]]$Q
  )
  
  
  extracted_data_player_4 <- data.frame(
    i = current_run$player_dfs[[4]]$i,
    t = current_run$player_dfs[[4]]$t,
    p = current_run$player_dfs[[4]]$p,
    q = current_run$player_dfs[[4]]$q,
    pi = current_run$player_dfs[[4]]$pi,
    Q = current_run$player_dfs[[4]]$Q
  )
  
  
  extracted_data_player_5 <- data.frame(
    i = current_run$player_dfs[[5]]$i,
    t = current_run$player_dfs[[5]]$t,
    p = current_run$player_dfs[[5]]$p,
    q = current_run$player_dfs[[5]]$q,
    pi = current_run$player_dfs[[5]]$pi,
    Q = current_run$player_dfs[[5]]$Q
  )
  # Zusammenfügen der extrahierten Daten zum finalen Dataframe
  final_df_player_1 <- rbind(final_df_player_1, extracted_data_player_1)
  final_df_player_2 <- rbind(final_df_player_2, extracted_data_player_2)
  final_df_player_3 <- rbind(final_df_player_3, extracted_data_player_3)
  final_df_player_4 <- rbind(final_df_player_4, extracted_data_player_4)
  final_df_player_5 <- rbind(final_df_player_5, extracted_data_player_5)
}

final_df = bind_rows(final_df_player_1,final_df_player_2, final_df_player_3,final_df_player_4,final_df_player_5)

saveRDS(object = final_df, file = "C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/gemini_results/Final_df/df_5_Firms_ALL_Observations_PRO.Rds")
# Der finale Dataframe "fin


final_df_plot_PRO = final_df %>% 
  group_by(i,t) %>% 
  summarize(q_mean = mean(q),
            p_mean = mean(p),
            pi_mean = mean(pi)) 

final_df_plot_Q_PRO = final_df %>% 
  group_by(t) %>% 
  summarize(Q = mean(Q))


final_df_plot_pi_PRO = final_df %>% 
  group_by(t) %>% 
  summarize(pi = mean(pi))


final_df_plot_Q$Q_PRO = final_df_plot_Q_PRO$Q
final_df_plot_pi$pi_PRO = final_df_plot_pi_PRO$pi











###Quantity




###Quantity





plot_q_5_Firms = ggplot(final_df_plot_Q, aes(x = t)) +
  geom_point(aes(y = Q, color = "Normal Version"), size = 3) +  # Punkte plotten für Q
  geom_line(aes(y = Q, color = "Normal Version"), size = 1) +  # Linie zwischen den Punkten für Q
  geom_hline(yintercept = 82.5, linetype = "dashed", color = "blue", size = 1) +  # Linie bei Q = 66
  geom_text(aes(x = max(t) * 0.9, y = 82.5, label = "Nash Quantity"), color = "blue", vjust = -1) +
  geom_hline(yintercept = 49.5, linetype = "dashed", color = "red", size = 1) +  # Linie bei Q = 49.5
  geom_text(aes(x = max(t) * 0.9, y = 49.5, label = "Collusive Quantity"), color = "red", vjust = -1) +
  geom_point(aes(y = Q_PRO, color = "Pro Version"), size = 3, shape = 17) +  # Zusätzlicher Scatterplot für Q_Pro
  geom_line(aes(y = Q_PRO, color = "Pro Version"), size = 1) +  # Linie für Q_Pro
  labs(
    x = "Round",  # Beschriftung der X-Achse
    y = "Average Quantity set",  # Beschriftung der Y-Achse
    color = "Version",  # Legende für die Gruppen
    title = "Average Quantity Set by Round",
    subtitle = "Oligopoly Experiment with 5 Firms and Different Gemini Versions"  # Titel des Plots
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(30, 225), breaks = c(30,40,60,80,100,125,200,225)) +  
  scale_x_continuous(breaks = c(1,5,10,15,20,25)) +  
  
  scale_color_manual(values = c("Normal Version" = "black", "Pro Version" = "green")) + 
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
    plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5),  # Stil des Untertitels und zentriert
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  )

# Plot anzeigen
print(plot_q_5_Firms)





###PI


plot_pi_5_firms = ggplot(final_df_plot_pi, aes(x = t)) +
  geom_point(aes(y = pi, color = "Normal Version"), size = 3) +  # Punkte plotten für pi
  geom_line(aes(y = pi, color = "Normal Version"), size = 1) +  # Linie für pi
  geom_hline(yintercept = 272, linetype = "dashed", color = "blue", size = 1) +  # Linie bei pi = 1089
  geom_text(aes(x = max(t) * 0.9, y = 235, label = "Nash Profits"), color = "blue", vjust = -1) +
  geom_hline(yintercept = 490, linetype = "dashed", color = "red", size = 1) +  # Linie bei pi = 1225.13
  geom_text(aes(x = max(t) * 0.9, y = 490, label = "Collusive Profits"), color = "red", vjust = -1) +
  geom_point(aes(y = pi_PRO, color = "Pro Version"), size = 3, shape = 17) +  # Punkte plotten für pi_PRO
  geom_line(aes(y = pi_PRO, color = "Pro Version"), size = 1) +  # Linie für pi_PRO
  labs(
    x = "Round",  # Beschriftung der X-Achse
    y = "Average Profits",  # Beschriftung der Y-Achse
    color = "Version",  # Legende für die Gruppen
    title = "Average Profits by Round",
    subtitle = "Oligopoly Experiment with 5 Firms and Different Gemini Versions"  # Untertitel des Plots
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(-50, 550), breaks = c(-50,0,100,200,300,400,500)) +  
  scale_x_continuous(breaks = c(1,5,10,15,20,25)) +  
  
  scale_color_manual(values = c("Normal Version" = "black", "Pro Version" = "green")) +  # Farben für die Versionen definieren
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Stil des Titels
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5)  # Stil des Untertitels und zentriert
  )

# Plot anzeigen
print(plot_pi_5_firms)


