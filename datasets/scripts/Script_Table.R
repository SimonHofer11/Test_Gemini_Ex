

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

