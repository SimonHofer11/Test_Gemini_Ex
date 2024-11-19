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















df_oligopol = readRDS("C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/gemini_results/Final_df/df_oligopol.Rds")
df_monopol = readRDS("C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/gemini_results/Final_df/df_monopol.Rds")



###Table Same Setting

df_oligopol_same_setting = df_oligopol %>% 
  mutate(same_setting = c(2,0,1,0,0,1,2,1,2))

df_oligopol_same_setting_pro_normal = df_oligopol_same_setting%>% 
  filter(same_setting >0)

df_oligopol_same_setting_pro_normal[2,1] = "Oligopoly 2 Firms 25 Rounds"

df_oligopol_same_setting_pro_normal[c(1, 2), ] <- df_oligopol_same_setting_pro_normal[c(2, 1), ]

df_oligopol_same_setting_pro_normal$Version = c("Normal","Pro","Normal","Pro","Normal","Pro")

 #df_oligopol = df_oligopol %>% 
#   mutate (pi_17_25 = (Q_mean_17_25/n)*(100-Q_mean_17_25)-(Q_mean_17_25/n))

# Assuming you have a dataframe named df_oligopol_same_setting_pro_normal

# Select only the needed columns
df_selected <- df_oligopol_same_setting_pro_normal[, c("n", "Version", "Q_nash", "Q_mean", "r", 
                                                       "Q_mean_17_25", "r_17_25", 
                                                       "pi_nash", "pi_collusive", 
                                                       "pi_avg_experiment")]

# Round specific columns to 2 decimal places
df_selected <- df_selected %>% 
  mutate(Q_nash = round(Q_nash,2),
         r = round(r,2),
         Q_mean = round(Q_mean,2),
         Q_mean_17_25 = round(Q_mean_17_25,2),
         r_17_25 = round(r_17_25,2),
         pi_avg_experiment = round(pi_avg_experiment,2))

# Rename the columns
colnames(df_selected) <- c("n Firms", "Version", "Q-Nash", "Q-Ø", "r", 
                           "Q-Ø 17-25", "r 17-25", "Pi-Nash", 
                           "Pi-Collusive", "Ø-Pi")





##########Mensch hinzufügen#################



df_human = df_final_all %>% 
  filter(type =="HUMAN")

df_human = df_human %>% 
  group_by(n) %>% 
  mutate(mean_Q = round(mean(Q),2),
         mean_pi = round(mean(pi),2))

df_human = df_human %>% 
  group_by(n) %>% 
  mutate(ratio_Nash = round(mean_Q/nash_Q,2)) %>% 
  ungroup()

df_human_17_25 = df_human %>% 
  filter(t >=17) %>% 
  group_by(n) %>% 
  summarize(mean_Q_17_25 = round(mean(Q),2))

df_human_17_25$Nash_Q = c(66,74.25,82.5)

df_human_17_25 = df_human_17_25 %>% 
  mutate(ratio_Nash_17_25 = round(mean_Q_17_25/Nash_Q,2))




df_human$mean_Q_17_25 <- ifelse(df_human$n == 2, 60.44, 
                                ifelse(df_human$n == 3, 72.59, 
                                       ifelse(df_human$n == 5, 88.43, NA)))

# Spalte ratio_Nash_17_25 hinzufügen basierend auf 'n'
df_human$ratio_Nash_17_25 <- ifelse(df_human$n == 2, 0.92, 
                                    ifelse(df_human$n == 3, 0.98, 
                                           ifelse(df_human$n == 5, 1.07, NA)))



df_human_selected = df_human %>% 
  group_by(n) %>% 
  summarize(Q_nash = mean(nash_Q),
            Q_avg = mean(mean_Q),
            r = mean(ratio_Nash),
            Q_avg_17_25 = mean(mean_Q_17_25),
            r_17_25 = mean(ratio_Nash_17_25))

df_human_selected$type = "Human"

colnames(df_human_selected) <- c("n Firms", "Q-Nash",  "Q-Ø", "r","Q-Ø 17-25", "r 17-25", "Version" 
                                  )





df_selected_short = df_selected %>% 
  select(c("n Firms", "Version", "Q-Nash", "Q-Ø", "r", 
           "Q-Ø 17-25", "r 17-25"))
         


df_ALL_summarized = rbind(df_selected_short,df_human_selected)

df_ALL_summarized <- df_ALL_summarized[order(df_ALL_summarized$n), ]


# Spalte 'Version' umbenennen zu 'Type'
colnames(df_ALL_summarized)[colnames(df_ALL_summarized) == "Version"] <- "Type"

# Werte in der Spalte 'Type' ändern: "Normal" -> "Flash"
df_ALL_summarized$Type[df_ALL_summarized$Type == "Normal"] <- "Flash"

df_ALL_summarized$Variation = c("Base","Base","Base Human","Base","Base","Base Human","Base","Base","Base Human")
#Es fehlen die einzelnen Variantne.


df_oligopol_subset <- df_oligopol[c(2, 4, 5), 
                                  c("name", "Q_nash", "Q_mean", "r", "Q_mean_17_25", "r_17_25", "n")]


df_oligopol_subset$Type = c("Flash")


df_oligopol_subset$Variation = c("Self Calculation","No History","Temperature 0")
df_oligopol_subset <- df_oligopol_subset[, !colnames(df_oligopol_subset) %in% "name"]
colnames(df_oligopol_subset) = c("Q-Nash","Q-Ø",  "r", "Q-Ø 17-25", "r 17-25","n Firms", "Variation", "Type"  
                                 )


df_ALL_summarized = rbind(df_ALL_summarized,df_oligopol_subset)

df_ALL_summarized <- df_ALL_summarized[order(df_ALL_summarized$n, df_ALL_summarized$Type), ]

colnames(df_ALL_summarized) <- c(
  "Number Firms", 
  "Type",
  "Q Nash", 
  expression(bar(Q)), # Q mit Querbalken
  "r",
  expression(bar(Q) ~ "17-25"), # Q mit Querbalken und Zusatz
  "r 17-25",
  "Variation"
)


#saveRDS(df_ALL_summarized, "C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/datasets/table_data/df_tables.Rds")



#########2Firms###########






df_2 <- df_ALL_summarized[df_ALL_summarized$`Number Firms`== 2, ]


df_2[c(5, 6), ] <- df_2[c(6, 5), ]



library(gt)


# Spaltenname mit HTML-Tags anpassen
colnames(df_2)[4] <- "Q̄"  # Setze direkt das Symbol für Q̄
colnames(df_2)[6] <- "Q̄ 17-25"  # Setze direkt das Symbol für Q̄


df_2

df_2 <- df_2[, -1]

# Tabelle erstellen
library(gt)

gt_table <- gt(df_2) %>%
  # Titel hinzufügen
  tab_header(
    title = "Oligopoly Experiment 2 Firms with Different Variations"
  ) %>%
  # Spalten formatieren: auf 2 Dezimalstellen runden
  fmt_number(
    columns = c(`Q Nash`, `Q̄`, r, `Q̄ 17-25`, `r 17-25`),
    decimals = 2
  ) %>%
  # Spaltennamen fett formatieren
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  # Werte in den Spalten "Type", "Q Nash", und "Variation" fett formatieren
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = c(Type, `Q Nash`, Variation))
  ) %>%
  # Werte in Zeile 2, Spalte 4 und Spalte 6 fett formatieren
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = 2, columns = c(r, `r 17-25`))
  ) %>%
  # Linien zwischen den Spalten und Zeilen hinzufügen
  tab_options(
    table_body.hlines.color = "gray",  # Horizontale Linien
    table_body.vlines.color = "gray",  # Vertikale Linien
    column_labels.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    table.border.top.color = "black",
    table.border.bottom.color = "black"
  ) %>%
  # Zusätzliche Stile anwenden
  tab_options(
    table.font.size = 12,
    column_labels.background.color = "gray90",
    column_labels.font.size = 14
  )

# Tabelle anzeigen
print(gt_table)






#########3Firms############

df_3 <- df_ALL_summarized[df_ALL_summarized$`Number Firms`== 3, ]


df_3[c(2, 3), ] <- df_3[c(3, 2), ]



library(gt)


# Spaltenname mit HTML-Tags anpassen
colnames(df_3)[4] <- "Q̄"  # Setze direkt das Symbol für Q̄
colnames(df_3)[6] <- "Q̄ 17-25"  # Setze direkt das Symbol für Q̄


df_3

df_3 <- df_3[, -1]

# Tabelle erstellen
library(gt)

gt_table_3 <- gt(df_3) %>%
  # Titel hinzufügen
  tab_header(
    title = "Oligopoly Experiment 3 Firms with Different Variations"
  ) %>%
  # Spalten formatieren: auf 2 Dezimalstellen runden
  fmt_number(
    columns = c(`Q Nash`, `Q̄`, r, `Q̄ 17-25`, `r 17-25`),
    decimals = 2
  ) %>%
  # Spaltennamen fett formatieren
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  # Werte in den Spalten "Type", "Q Nash", und "Variation" fett formatieren
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = c(Type, `Q Nash`, Variation))
  ) %>%
  # Werte in Zeile 2, Spalte 4 und Spalte 6 fett formatieren
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = 3, columns = c(r, `r 17-25`))
  ) %>%
  # Linien zwischen den Spalten und Zeilen hinzufügen
  tab_options(
    table_body.hlines.color = "gray",  # Horizontale Linien
    table_body.vlines.color = "gray",  # Vertikale Linien
    column_labels.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    table.border.top.color = "black",
    table.border.bottom.color = "black"
  ) %>%
  # Zusätzliche Stile anwenden
  tab_options(
    table.font.size = 12,
    column_labels.background.color = "gray90",
    column_labels.font.size = 14
  )

# Tabelle anzeigen
print(gt_table_3)








#########5Firms############

df_5 <- df_ALL_summarized[df_ALL_summarized$`Number Firms`== 5, ]


df_5[c(2, 3), ] <- df_5[c(3, 2), ]



library(gt)


# Spaltenname mit HTML-Tags anpassen
colnames(df_5)[4] <- "Q̄"  # Setze direkt das Symbol für Q̄
colnames(df_5)[6] <- "Q̄ 17-25"  # Setze direkt das Symbol für Q̄


df_5

df_5 <- df_5[, -1]

# Tabelle erstellen
library(gt)

gt_table_5 <- gt(df_5) %>%
  # Titel hinzufügen
  tab_header(
    title = "Oligopoly Experiment 5 Firms with Different Variations"
  ) %>%
  # Spalten formatieren: auf 2 Dezimalstellen runden
  fmt_number(
    columns = c(`Q Nash`, `Q̄`, r, `Q̄ 17-25`, `r 17-25`),
    decimals = 2
  ) %>%
  # Spaltennamen fett formatieren
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  # Werte in den Spalten "Type", "Q Nash", und "Variation" fett formatieren
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = c(Type, `Q Nash`, Variation))
  ) %>%
  # Werte in Zeile 2, Spalte 4 und Spalte 6 fett formatieren
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = 1, columns = c(r, `r 17-25`))
  ) %>%
  # Linien zwischen den Spalten und Zeilen hinzufügen
  tab_options(
    table_body.hlines.color = "gray",  # Horizontale Linien
    table_body.vlines.color = "gray",  # Vertikale Linien
    column_labels.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    table.border.top.color = "black",
    table.border.bottom.color = "black"
  ) %>%
  # Zusätzliche Stile anwenden
  tab_options(
    table.font.size = 12,
    column_labels.background.color = "gray90",
    column_labels.font.size = 14
  )

# Tabelle anzeigen
print(gt_table_5)









##########Monopol###########
df_monopol = readRDS("C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/gemini_results/Final_df/df_monopol.Rds")

df_monopol = df_monopol %>% 
  select(name,Q_nash,Q_mean,r,Q_mean_17_25,r_17_25)
colnames(df) = c("Q_nash","Q_mean","r","Q_mean_17_25","r_17_25","name")


df_monopol = rbind(df_monopol,df)


df_monopol$name = c("Base","Profit Maximum Formula","Support","Temperature 0", "Base")

df_monopol$Type = c("Flash","Flash","Flash","Flash","Pro")
df_monopol$Variation = df_monopol$name




df_monopol[c(2, 3), ] <- df_monopol[c(3, 2), ]

# Entferne die Spalte "name"
df_monopol <- df_monopol[, !colnames(df_monopol) %in% "name"]

# Spalten neu anordnen
df_monopol <- df_monopol[, c("Type", "Q_nash", "Q_mean", "r", "Q_mean_17_25", "r_17_25", "Variation")]


library(gt)

colnames(df_monopol) = c("Type","Q Nash","Q̄","r","Q̄ 17-25","r 17-25", "Variation")
# Spaltenname mit HTML-Tags anpassen



# Tabelle erstellen
library(gt)

gt_table_monopol <- gt(df_monopol) %>%
  # Titel hinzufügen
  tab_header(
    title = "Monopoly Experiment with Different Variations"
  ) %>%
  # Spalten formatieren: auf 2 Dezimalstellen runden
  fmt_number(
    columns = c(`Q Nash`, `Q̄`, r, `Q̄ 17-25`, `r 17-25`),
    decimals = 2
  ) %>%
  # Spaltennamen fett formatieren
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  # Werte in den Spalten "Type", "Q Nash", und "Variation" fett formatieren
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = c(Type, `Q Nash`, Variation))
  ) %>%
  # Werte in Zeile 2, Spalte 4 und Spalte 6 fett formatieren
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = c(2), columns = c(r, `r 17-25`))
  ) %>%
  # Linien zwischen den Spalten und Zeilen hinzufügen
  tab_options(
    table_body.hlines.color = "gray",  # Horizontale Linien
    table_body.vlines.color = "gray",  # Vertikale Linien
    column_labels.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    table.border.top.color = "black",
    table.border.bottom.color = "black"
  ) %>%
  # Zusätzliche Stile anwenden
  tab_options(
    table.font.size = 12,
    column_labels.background.color = "gray90",
    column_labels.font.size = 14
  )

# Tabelle anzeigen
print(gt_table_monopol)


#saveRDS(df_monopol, "C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/datasets/table_data/df_tables_monopoly.Rds")




















###############FRÜHER################


# Load the necessary package
library(kableExtra)

# Make specific cells bold
df_selected[1, 4] <- cell_spec(df_selected[1, 4], bold = TRUE)
df_selected[1, 5] <- cell_spec(df_selected[1, 5], bold = TRUE)
df_selected[1, 6] <- cell_spec(df_selected[1, 6], bold = TRUE)
df_selected[1, 7] <- cell_spec(df_selected[1, 7], bold = TRUE)
df_selected[2, 10] <- cell_spec(df_selected[2, 10], bold = TRUE)

df_selected[3, 4] <- cell_spec(df_selected[3, 4], bold = TRUE)
df_selected[3, 5] <- cell_spec(df_selected[3, 5], bold = TRUE)
df_selected[3, 6] <- cell_spec(df_selected[3, 6], bold = TRUE)
df_selected[3, 7] <- cell_spec(df_selected[3, 7], bold = TRUE)
df_selected[4, 10] <- cell_spec(df_selected[4, 10], bold = TRUE)

df_selected[5, 4] <- cell_spec(df_selected[5, 4], bold = TRUE)
df_selected[5, 5] <- cell_spec(df_selected[5, 5], bold = TRUE)
df_selected[5, 6] <- cell_spec(df_selected[5, 6], bold = TRUE)
df_selected[5, 7] <- cell_spec(df_selected[5, 7], bold = TRUE)
df_selected[6, 10] <- cell_spec(df_selected[6, 10], bold = TRUE)

# Create a formatted table with escape = FALSE to allow HTML rendering
kable(df_selected, format = "html", escape = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F, position = "center") %>%
  row_spec(0, bold = T, extra_css = "border-bottom: 2px solid #000;") %>%  # Add a bold header with a line underneath
  row_spec(seq(2, nrow(df_selected), 2), extra_css = "border-bottom: 2px solid #000;") %>%  # Add a line after every second row
  row_spec(0, bold = T) %>%  # Make the header bold without background color
  add_header_above(c("Oligopoly Experiment Results" = ncol(df_selected)), bold = T, align = "c", font_size = 16) %>%
  column_spec(1:ncol(df_selected), border_right = TRUE) %>%  # Add vertical lines between columns
  column_spec(1, bold = T) %>%  # Make the content of the first column (Number of Firms) bold
  column_spec(2, bold = T) %>%  # Make the content of the second column (Version) bold
  column_spec(3, bold = T,color = "green") %>%  # Color the third column
  column_spec(4, background = "#FFE0B2") %>%  # Color the fourth column
  column_spec(5, background = "#C5CAE9") %>%  # Color the fifth column
  column_spec(6, background = "#FFCDD2") %>%  # Color the sixth column
  column_spec(7, background = "#D1C4E9") %>%  # Color the seventh column
  column_spec(8, bold = T,color = "green") %>%  # Color the eighth column
  column_spec(9, bold = T,color = "green")   %>% 
  column_spec(10, background = "lightgreen")# Color the ninth column








library(dplyr)





####################HEREREREER




################Table all Experiments 2 Firms
df_oligopol = readRDS("C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/gemini_results/Final_df/df_oligopol.Rds")

df_oligopol_2_Firms_Variations = df_oligopol[1:5,]


# Reorder the rows: move the 4th row to the 1st position and the 1st row to the 5th position
df_oligopol_2_Firms_Variations <- df_oligopol_2_Firms_Variations[c(4, 2, 3, 5, 1), ]



# Select only the needed columns
df_selected <- df_oligopol_2_Firms_Variations[, c("Q_nash", "Q_mean", "r", 
                                                       "Q_mean_17_25", "r_17_25", 
                                                       "pi_nash", "pi_collusive", 
                                                       "pi_avg_experiment")]



# Assuming df_oligopol_2_Firms_Variations is already defined and rows reordered
# Assuming df_oligopol_2_Firms_Variations is already defined and rows reordered


df_selected$Variation = c("W/o previous Strategy","Self Calculation","With previous Strategy","Temperature = 0","Pro Version")
df_selected <- df_selected[, c("Variation", setdiff(names(df_selected), "Variation"))]


# Round specific columns to 2 decimal places
df_selected <- df_selected %>% 
  mutate(Q_nash = round(Q_nash,2),
         r = round(r,2),
         Q_mean = round(Q_mean,2),
         Q_mean_17_25 = round(Q_mean_17_25,2),
         r_17_25 = round(r_17_25,2),
         pi_avg_experiment = round(pi_avg_experiment,2))

# Rename the columns
colnames(df_selected) <- c("Variation","Q-Nash", "Q-Ø", "r", 
                           "Q-Ø 17-25", "r 17-25", "Pi-Nash", 
                           "Pi-Collusive", "Ø-Pi") 


# Make specific cells bold
df_selected[2, 3] <- cell_spec(df_selected[2, 3], bold = TRUE, color = "green")
df_selected[2, 4] <- cell_spec(df_selected[2, 4], bold = TRUE, color = "green")

df_selected[2, 5] <- cell_spec(df_selected[2, 5], bold = TRUE, color = "green")
df_selected[2, 6] <- cell_spec(df_selected[2, 6], bold = TRUE, color = "green")

df_selected[5, 3] <- cell_spec(df_selected[5, 3], bold = TRUE, color = "red")
df_selected[5, 4] <- cell_spec(df_selected[5, 4], bold = TRUE, color = "red")

df_selected[5, 5] <- cell_spec(df_selected[5, 5], bold = TRUE, color = "red")
df_selected[5, 6] <- cell_spec(df_selected[5, 6], bold = TRUE, color = "red")
df_selected[5, 9] <- cell_spec(df_selected[5, 9], bold = TRUE)





# Create a formatted table with escape = FALSE to allow HTML rendering
kable(df_selected, row.names = F, format = "html", escape = FALSE, caption = "<b style='text-align:center;'>Oligopoly Experiment 2 Firms with Different Variations</b>") %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F, position = "center") %>%
  row_spec(0, bold = T, extra_css = "border-bottom: 2px solid #000;") %>%  # Add a bold header with a line underneath
  #add_header_above(c(" " = 1, "Oligopoly Experiment Results" = ncol(df_selected)), bold = T, align = "c", font_size = 16) %>%  # Adjusted for 9 columns
  column_spec(1:ncol(df_selected), border_right = TRUE) %>%
  #column_spec(1, bold = T) %>%
  column_spec(2, bold = T) %>%
  column_spec(3, background = "#FFE0B2") %>%
  column_spec(4, background = "#E1BEE7") %>%
  column_spec(5, background = "#C5CAE9") %>%
  column_spec(6, background = "#FFCDD2") %>%
  column_spec(7, bold = T) %>%
  column_spec(8, bold = T) %>%
  
  column_spec(9, background = "lightblue") %>%
  footnote(general = "Numbers in green are those closest to the Nash equilibrium, in red, those closest to the collusive equilibrium.")
























# Assuming df_oligopol_2_Firms_Variations is already defined and rows reordered
df_oligopol = readRDS("C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/gemini_results/Final_df/df_oligopol.Rds")



df_oligopol_2_Firms_Variations = df_oligopol[1:5,]


# Reorder the rows: move the 4th row to the 1st position and the 1st row to the 5th position
df_oligopol_2_Firms_Variations <- df_oligopol_2_Firms_Variations[c(4, 2, 3, 5, 1), ]



# Select only the needed columns
df_selected <- df_oligopol_2_Firms_Variations[, c("Q_nash", "Q_mean", "r", 
                                                  "Q_mean_17_25", "r_17_25", 
                                                  "pi_nash", "pi_collusive", 
                                                  "pi_avg_experiment")]


# Add a new column "Variation"
df_selected$Variation = c("W/o previous Strategy","Self Calculation","With previous Strategy","Temperature = 0","Pro Version")
df_selected <- df_selected[, c("Variation", setdiff(names(df_selected), "Variation"))]

# Round specific columns to 2 decimal places
df_selected <- df_selected %>% 
  mutate(Q_nash = round(Q_nash,2),
         r = round(r,2),
         Q_mean = round(Q_mean,2),
         Q_mean_17_25 = round(Q_mean_17_25,2),
         r_17_25 = round(r_17_25,2),
         pi_avg_experiment = round(pi_avg_experiment,2))

# Rename the columns
colnames(df_selected) <- c("Variation", "Q-Nash", "Q-Mean", "r", 
                           "Q-Mean 17-25", "r 17-25", "Pi-Nash", 
                           "Pi-Collusive", "Ø Pi-Experiment") 

# Make specific cells bold
df_selected[2, 4] <- cell_spec(df_selected[2, 4], bold = TRUE)
df_selected[2, 6] <- cell_spec(df_selected[2, 6], bold = TRUE)
df_selected[3, 3] <- cell_spec(df_selected[3, 3], bold = TRUE)
df_selected[3, 5] <- cell_spec(df_selected[3, 5], bold = TRUE)
df_selected[5, 9] <- cell_spec(df_selected[5, 9], bold = TRUE)  # Row 5 instead of 6

# Create a formatted table with escape = FALSE to allow HTML rendering, and remove row names (index)
kable(df_selected, format = "html", escape = FALSE, row.names = FALSE, 
      caption = "<b style='text-align:center;'>Oligopoly Experiment 2 Firms with Different Variations</b>") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F, position = "center", font_size = 16) %>%
  
  row_spec(0, bold = T, extra_css = "border-bottom: 2px solid #000;") %>%
  add_header_above(c("Oligopoly Experiment Results" = ncol(df_selected)), bold = T, align = "c", font_size = 16) %>%
  
  column_spec(1:ncol(df_selected), border_right = TRUE) %>%
  column_spec(1, bold = T) %>%
  column_spec(3, background = "#FFE0B2") %>%
  column_spec(4, background = "#E1BEE7") %>%
  column_spec(5, background = "#C5CAE9") %>%
  column_spec(6, background = "#FFCDD2") %>%
  column_spec(7, background = "#D1C4E9") %>%
  column_spec(8, background = "#B3E5FC")






















##########Monopol
df_monopol = readRDS("C:/Users/Simon Hofer/OneDrive/Dokumente/Master/Semesterverzeichnis/Semester 4/Github/Gemini-Ex/gemini_results/Final_df/df_monopol.Rds")

df_monopol = df_monopol %>% 
  mutate(pi_avg_17_25_exp = Q_mean_17_25*(100-Q_mean_17_25)-Q_mean_17_25)


# Reorder the rows: move the 4th row to the 1st position and the 1st row to the 5th position
df_monopol <- df_monopol[c(1,3,2,4), ]



# Select only the needed columns
df_selected <- df_monopol[, c("Q_nash", "Q_mean", "r", 
                                                  "Q_mean_17_25", "r_17_25", 
                                                  "pi_max", 
                                                  "pi_avg_experiment","pi_avg_17_25_exp")]


# Add a new column "Variation"
df_selected$Variation = c("Base Prompt W/o Support", "With Profit Calculator","With Profit Maximum Formula","Temperature = 0")
df_selected <- df_selected[, c("Variation", setdiff(names(df_selected), "Variation"))]

# Round specific columns to 2 decimal places
df_selected <- df_selected %>% 
  mutate(Q_nash = round(Q_nash,2),
         r = round(r,2),
         Q_mean = round(Q_mean,2),
         Q_mean_17_25 = round(Q_mean_17_25,2),
         r_17_25 = round(r_17_25,2),
         pi_avg_experiment = round(pi_avg_experiment,2),
         pi_avg_17_25_exp = round(pi_avg_17_25_exp,2))

# Rename the columns
colnames(df_selected) <- c("Variation", "Q-Opt", "Q-Ø", "r", 
                           "Q-Ø 17-25", "r 17-25", "Pi-Max", "Ø-Pi","Ø-Pi 17-25") 

# Make specific cells bold
df_selected[2, 3] <- cell_spec(df_selected[2, 3], bold = TRUE)

df_selected[2, 4] <- cell_spec(df_selected[2, 4], bold = TRUE)
df_selected[3, 4] <- cell_spec(df_selected[3, 4], bold = TRUE)
df_selected[2, 6] <- cell_spec(df_selected[2, 6], bold = TRUE)
df_selected[2, 8] <- cell_spec(df_selected[2, 8], bold = TRUE)
df_selected[2, 9] <- cell_spec(df_selected[2, 9], bold = TRUE)
df_selected[2, 5] <- cell_spec(df_selected[2,5], bold = TRUE)


# Create a formatted table with escape = FALSE to allow HTML rendering, and remove row names (index)
kable(df_selected, format = "html", escape = FALSE, row.names = FALSE, 
      caption = "<b style='text-align:center;'>Monopoly Experiment with Different Variations</b>") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F, position = "center", font_size = 16) %>%
  
  row_spec(0, bold = T, extra_css = "border-bottom: 2px solid #000;") %>%
  column_spec(1:ncol(df_selected), border_right = TRUE) %>%
  column_spec(1, bold = T) %>%
  column_spec(3, background = "#FFE0B2") %>%
  column_spec(2, color = "green",bold = T) %>% 
  column_spec(4, background = "#E1BEE7") %>%
  column_spec(5, background = "#C5CAE9") %>%
  column_spec(6, background = "#FFCDD2") %>%
  column_spec(7, color = "green",bold = T) %>%
  column_spec(8, background = "#B3E5FC") %>% 
  column_spec(9, background = "lightgreen")

