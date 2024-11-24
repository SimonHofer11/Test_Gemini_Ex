#Skript zur Berechnung der Datenanalyse
getwd()


# Benötigte Pakete laden
library(readr)
library(dplyr)
library(tidyr)


#4 times Pro, 1 Time Flash
df = read.csv("C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/gemini_results/Oligopoly/mixed_4_Pro_1_Flash/df_5_mixed_4_pro_1_flash_all_rounds.csv")





df_aggregated_round_type= df %>% 
  group_by(t,player_type) %>% 
  summarize(pi = round(mean(pi),2),
            q = round(mean(q),2),
            Q = round(mean(Q),2),
            p = round(mean(p),2))


  
  # Setze i = 6, wenn player_type == "FLASH"
df_i = df
df_i$i[df_i$player_type == "FLASH"] <- 6

# Setze i = 5 für Beobachtungen 26-50
df_i$i[26:50] <- 5

# Setze i = 4 für Beobachtungen 151-175
df_i$i[151:175] <- 4
df_i$i[df_i$player_type == "FLASH"] <- 2



df_agg_t_i_type = df_i %>% 
  group_by(t,i,player_type)%>% 
  summarize(pi = round(mean(pi),2),
            q = round(mean(q),2),
            Q = round(mean(Q),2),
            p = round(mean(p),2))


library(ggplot2)

# Erstellen einer neuen Spalte, um "Version" direkt zu definieren
df_agg_t_i_type$version <- ifelse(df_agg_t_i_type$i == 2, "FLASH", "PRO")

# Liniendiagramm mit den gewünschten Änderungen
Q = ggplot(df_agg_t_i_type, aes(x = t, y = q, group = i, color = version)) +
  geom_line(linewidth = 1) +  # Verwende `linewidth` für die Linienstärke
  scale_color_manual(
    values = c(
      "PRO" = "gray",   # Grau für PRO
      "FLASH" = "blue"  # Blau für FLASH
    )
  ) +
  labs(
    title = "Average Quantity by Round",               # Titel
    subtitle = "Oligopoly Experiment with Mixed Version: 4 Pro Player, 1 Flash Player",  # Untertitel
    x = "Round",                                       # X-Achsenbeschriftung
    y = "Average Quantity",                            # Y-Achsenbeschriftung
    color = "Version"                                  # Legendenbeschriftung
  ) +
  theme_minimal() +                                    # Minimalistisches Theme
  theme(
    legend.position = "right",                        # Legende rechts
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Titel zentrieren
    plot.subtitle = element_text(size = 12, hjust = 0.5),              # Untertitel zentrieren
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )



PI = ggplot(df_agg_t_i_type, aes(x = t, y = pi, group = i, color = version)) +
  geom_line(linewidth = 1) +  # Verwende `linewidth` für die Linienstärke
  scale_color_manual(
    values = c(
      "PRO" = "gray",   # Grau für PRO
      "FLASH" = "blue"  # Blau für FLASH
    )
  ) +
  labs(
    title = "Average Profit by Round",               # Titel
    subtitle = "Oligopoly Experiment with Mixed Version: 4 Pro Player, 1 Flash Player",  # Untertitel
    x = "Round",                                       # X-Achsenbeschriftung
    y = "Average Profit",                            # Y-Achsenbeschriftung
    color = "Version"                                  # Legendenbeschriftung
  ) +
  theme_minimal() +                                    # Minimalistisches Theme
  theme(
    legend.position = "right",                        # Legende rechts
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Titel zentrieren
    plot.subtitle = element_text(size = 12, hjust = 0.5),              # Untertitel zentrieren
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )



library(patchwork)



join = Q / PI
join

